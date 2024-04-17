#pragma once

// Entity component system

#include "arraylist.h"

#include <type_traits>
#include <tuple>
#include <vector>
#include <functional>
#include <unordered_map>
#include <algorithm>
#include <span>
#include <shared_mutex>
#include <thread>
#include <mutex>

#ifdef _MSC_VER
#define SLIMECS_FUNCTION_NAME __FUNCSIG__
#else
#define SLIMECS_FUNCTION_NAME __PRETTY_FUNCTION__
#endif

#ifndef SLIMECS_DEBUG_INFO
#if _DEBUG
// We keep extra debug info stored with each entity so the debug visualiser has enough information to do lookups
#define SLIMECS_DEBUG_INFO 1
#endif
#endif

#ifndef SLIMECS_ASSERT
#include <cassert>
#define SLIMECS_ASSERT assert
#endif

//#define SLIMECS_COMPONENT(__NAME) extern template const slimecs::ECSComponentID slimecs::ECSComponentType<__NAME>::m_componentID

// Variable definitions and code to register a component on startup. Place once per component type in a source file!
#define SLIMECS_COMPONENT_DEFINE(__NAME) template<> const slimecs::ECSComponentID slimecs::ECSComponentType<__NAME>::m_componentID = \
	slimecs::ECSManager::RegisterComponentType<__NAME>(slimecs::ECSManager::ComponentTypeToID<__NAME>(#__NAME))

namespace slimecs
{
	const int kECSSliceSize = 1024;

	typedef unsigned int ecshash_t;
	typedef arraylist_data ecscontainerbase_t;
	typedef unsigned short ecsversion_t;
	typedef unsigned short ecsarchetype_index_t;

	template<class T>
	using ecscontainerview_t = arraylist_view<T, kECSSliceSize>;

	const ecscontainerbase_t::size_type kInvalidECSIndex = 0x0000ffffffffffff;
	const ecsarchetype_index_t kInvalidArchetypeIndex = ~(ecsarchetype_index_t) 0;

	class ECSManager;
	struct ECSChunk;

	inline static constexpr ecshash_t hash_fnv1a_const(const char* const s, const ecshash_t value = 0x811c9dc5) noexcept {
		return !*s ? value : hash_fnv1a_const(s + 1, (value ^ ecshash_t(*s)) * 0x1000193);
	}

	struct ECSComponentID
	{
		const ecshash_t m_hash;
		const char* m_name;
	};

	// Internal class for ecs manager registration
	struct IECSComponentType
	{
		virtual ~IECSComponentType() {};
		virtual void Grow(ECSChunk* chunk) = 0;
		virtual void GrowMove(ECSChunk* srcChunk, ECSChunk* targetChunk, ecscontainerbase_t::size_type srcIndex) = 0;
		virtual void Remove(ECSChunk* chunk, ecscontainerbase_t::size_type index) = 0;
		virtual void Clear(ECSChunk* chunk) = 0;
	};

	// An instance consisting of components
	struct ECSInstance
	{
		typedef ecscontainerbase_t::size_type id_t;

		union
		{
			struct
			{
				id_t m_index : 48; // An index pointing to where the components are in each of the chunks
				id_t m_version : 16; // ecsversion_t, type must match so bitfield works - Keeps track of the current 'version' of the id. Gets bumped when the id is added/removed so we don't use outdated ids
			} m_parts;
			id_t m_id;
		};

		ECSInstance()
		{ 
			m_parts.m_index = kInvalidECSIndex;
			m_parts.m_version = 0;
		}

#if SLIMECS_DEBUG_INFO
		ECSManager* m_ecsManager = nullptr;
#endif

		explicit ECSInstance(id_t t, ECSManager* ecsManager = nullptr)
#if SLIMECS_DEBUG_INFO
			: m_ecsManager(ecsManager)
#endif
		{
			m_id = t;

#if !SLIMECS_DEBUG_INFO
			static_assert(sizeof(*this) == sizeof(m_id));
#endif
		}

		ECSInstance(id_t index, ecsversion_t version, ECSManager* ecsManager = nullptr)
#if SLIMECS_DEBUG_INFO
			: m_ecsManager(ecsManager)
#endif
		{
			m_parts.m_index = index;
			m_parts.m_version = version;

#if !SLIMECS_DEBUG_INFO
			static_assert(sizeof(*this) == sizeof(m_parts));
#endif
		}

		constexpr bool IsValid() const
		{
			return m_parts.m_index != kInvalidECSIndex;
		}

		constexpr operator bool() const
		{
			return IsValid();
		}

		constexpr id_t Index() const
		{
			return m_parts.m_index;
		}

		constexpr id_t Version() const
		{
			return m_parts.m_version;
		}

		constexpr bool operator<(const ECSInstance& o) const 
		{
			return std::tie(m_parts.m_index, m_parts.m_version) < std::tie(o.m_parts.m_index, o.m_parts.m_version);
		}

		constexpr bool operator==(const ECSInstance& o) const 
		{
			return m_parts.m_index == o.m_parts.m_index && m_parts.m_version == o.m_parts.m_version;
		}

		constexpr operator id_t() const
		{
			return m_id;
		}
	};

	const ECSInstance kInvalidECSInstance(kInvalidECSIndex, nullptr);

	// Holds all components of a specific type, for a specific archetype
	struct ECSChunk
	{
		const ECSComponentID m_componentID;
		ecscontainerbase_t m_data;
		IECSComponentType* m_type;
	
		inline ECSChunk(const ECSComponentID& componentID);
		inline ~ECSChunk();
		inline void Grow();
		inline void Remove(ecscontainerbase_t::size_type index);
		inline void Clear();
	};

	template <class T>
	struct ECSComponentType : public IECSComponentType
	{
		typedef T value_type;

		ECSComponentType()
		{
			// Hack to force the Get() to be generated
			auto p = (volatile void*)Get; static_cast<void>(p);
		}

		template<typename... Args>
		void Grow(ECSChunk* chunk, Args&&... args)
		{
			ecscontainerview_t<T>(chunk->m_data).emplace_back(std::forward<Args>(args)...);
		}

		void Grow(ECSChunk* chunk) override
		{
			if constexpr (std::is_default_constructible_v<T>)
			{
				ecscontainerview_t<T>(chunk->m_data).emplace_back();
			}
			else
			{
				// Create a component with uninitialized memory as fallback
				ecscontainerview_t<T>(chunk->m_data).reserve_back();
				assert("Cannot default construct component type");
			}
		}

		void GrowMove(ECSChunk* srcChunk, ECSChunk* targetChunk, ecscontainerbase_t::size_type srcIndex) override
		{
			Grow(targetChunk, std::move(ecscontainerview_t<T>(srcChunk->m_data)[srcIndex]));
		}

		void Remove(ECSChunk* chunk, ecscontainerbase_t::size_type index) override
		{
			ecscontainerview_t<T>(chunk->m_data).erase(index);
		}

		void Clear(ECSChunk* chunk) override
		{
			ecscontainerview_t<T>(chunk->m_data).clear();
		}

		static constexpr ecshash_t GetHash()
		{
			constexpr ecshash_t v = hash_fnv1a_const(SLIMECS_FUNCTION_NAME);

			return v;
		}

		const static ECSComponentID m_componentID;

	private:
		// Used by extensions/debugger for reliable resolving
		static constexpr T& Get(ECSChunk* chunk, ecscontainerbase_t::size_type index)
		{
			return ecscontainerview_t<T>(chunk->m_data)[index];
		}
	};

	struct ECSAlgorithm
	{
		template <typename T, typename = void>
		struct is_chunk_type : std::false_type {};

		template <typename T>
		struct is_chunk_type<T, decltype((void) ECSComponentType<T>::m_componentID, void())> : std::true_type {};

		template<bool, typename T>
		struct pick
		{
			using type = std::tuple<>;
		};

		template<typename T>
		struct pick<true, T>
		{
			using type = std::tuple<T>;
		};
	};

	// Defines a group of component types
	class ECSArchetype
	{
	private:
		ECSArchetype(ecshash_t hash, const std::span<const ECSComponentID>& components)
			: m_typehash(hash)
			, m_chunks(components.begin(), components.end())
		{ 
		}

		ECSChunk* GetChunkByHash(ecshash_t hash)
		{
			for (auto& chunk : m_chunks)
			{
				if (chunk.m_componentID.m_hash == hash)
				{
					return &chunk;
				}
			}

			return nullptr;
		}

		constexpr const ECSChunk* GetChunkByHash(ecshash_t hash) const
		{
			for (auto& chunk : m_chunks)
			{
				if (chunk.m_componentID.m_hash == hash)
				{
					return &chunk;
				}
			}

			return nullptr;
		}

		template<class T>
		ecscontainerview_t<T> GetChunkView()
		{
			auto pChunk = GetChunk<T>();
			assert(pChunk); // Chunk type did not exist on this archetype!

			return ecscontainerview_t<T>(pChunk->m_data);
		}

		template<class... Ts>
		static constexpr ecshash_t GetComponentsHash()
		{
			return (ECSComponentType<std::remove_cvref_t<Ts>>::GetHash() ^ ... ^ 0);
		}

		static constexpr ecshash_t CombineTypeHash(ecshash_t hash1, ecshash_t hash2)
		{
			return hash1 ^ hash2;
		}

		template<class T>
		ECSChunk* GetChunk()
		{
			return GetChunkByHash(ECSComponentType<T>::GetHash());
		}

		template<class T>
		void GrowChunkWithComponent(T&& component)
		{
			ECSChunk* pChunk = GetChunk<std::remove_cvref_t<T>>();
			auto pComponentType = static_cast<ECSComponentType<std::remove_cvref_t<T>>*>(pChunk->m_type);
			pComponentType->Grow(pChunk, std::forward<T>(component));
		}

		template<class... Ts>
		ecscontainerbase_t::size_type CreateInstance(ecscontainerbase_t::size_type idsIndex, Ts&&... components)
		{
			// Grow each chunk with the matching component
			(GrowChunkWithComponent(std::forward<Ts>(components)), ...);

			// Update bookkeeping
			return TrackNewInstance(idsIndex);
		}

		ecscontainerbase_t::size_type CreateInstance(ecscontainerbase_t::size_type idsIndex)
		{
			// Grow all chunks with their default constructors
			for (auto& chunk : m_chunks)
			{
				chunk.Grow();
			}

			// Update bookkeeping
			return TrackNewInstance(idsIndex);
		}

		ecscontainerbase_t::size_type TrackNewInstance(ecscontainerbase_t::size_type idsIndex)
		{
			// Reference back to the global list so we know what to update on destroy
			m_chunkToId.push_back(idsIndex);

			// Return index of new instance
			return m_chunkToId.size() - 1;
		}

		ecscontainerbase_t::size_type DestroyInstance(ecscontainerbase_t::size_type chunkIndex)
		{
			// Erase item from all chunks
			for (auto& chunk : m_chunks)
			{
				chunk.Remove(chunkIndex);
			}

			// Remove() moves the last instance in the chunks to the 'chunkIndex'
			// We need to modify our dataset to correct this, meaning:
			// - Update the m_chunkToId for that index
			// - Update the global id for that entry to point to the correct index again
			auto modifiedIds = m_chunkToId.back();
			m_chunkToId[chunkIndex] = modifiedIds;

			// Remove
			m_chunkToId.pop_back();

			// To update the global ID, we need to know 1) which ID to update 2) what to update it to
			// The first one we return here, the second one is the index we deleted
			return modifiedIds;
		}

	public:
		ecscontainerbase_t::size_type Count() const
		{
			return (ecscontainerbase_t::size_type)m_chunkToId.size();
		}

	protected:
		ecshash_t m_typehash;
		std::vector<ecscontainerbase_t::size_type> m_chunkToId;
		std::vector<ECSChunk> m_chunks;

	private:
		friend class ECSManager;
		friend class ECSMutation;
		template <class Filter, class... Ts> friend class ECSQuery;
		template <class... Ts> friend struct Filter;

	};

	class ECSComponentGroup
	{
	public:
		template <int D, class... Ns> 
		struct iterator_stack
		{
			iterator_stack(ECSChunk** chunks) { }
			iterator_stack() { }

			bool AtEnd() { return true; }
			iterator_stack& operator++() { return *this; }
		};

		// Since we cannot easily handle iterating through a variable number of strongly typed iterators, we have this helper class
		template <std::size_t Depth, class N, class... Ns> 
		struct iterator_stack<Depth, N, Ns...> : iterator_stack<Depth + 1, Ns...>
		{
		public:
			static constexpr std::size_t depth = Depth;
			typedef N type;

			iterator_stack(std::nullptr_t) { }

			iterator_stack(ECSChunk** chunks)
				: iterator_stack<Depth + 1, Ns...>(chunks + 1)
				, m_typed((*chunks)->m_data)
				, m_it(m_typed.begin())
				, m_end(m_typed.end()) { }

			iterator_stack() { }
			~iterator_stack() { }

			iterator_stack(iterator_stack&& other) noexcept = delete;

			iterator_stack(const iterator_stack& other)
				: iterator_stack<Depth + 1, Ns...>(other)
			{
				if (other.m_typed.has_data())
				{
					auto distance = std::distance<typename ecscontainerview_t<N>::const_iterator>(other.m_typed.begin(), other.m_it);

					m_typed = other.m_typed;
					m_it = m_typed.begin() + distance;
					m_end = m_typed.end();
				}
			}

			iterator_stack& operator=(iterator_stack&& other) noexcept
			{
				if (other.m_typed.has_data())
				{
					auto distance = std::distance<typename ecscontainerview_t<N>::const_iterator>(other.m_typed.begin(), other.m_it);

					m_typed = std::move(other.m_typed);
					m_it = m_typed.begin() + distance;
					m_end = m_typed.end();
				}
				else
				{
					// Reset to default
					m_typed = decltype(m_typed)();
					m_it = decltype(m_it)();
					m_end = decltype(m_end)();
				}
				
				*Base() = std::move(*other.Base());

				return *this;
			}

			void operator=(const iterator_stack<Depth, N, Ns...>& other)
			{
				if (other.m_typed.has_data())
				{
					auto distance = std::distance<typename ecscontainerview_t<N>::const_iterator>(other.m_typed.begin(), other.m_it);

					m_typed = other.m_typed;
					m_it = m_typed.begin() + distance;
					m_end = m_typed.end();
				}
				else
				{
					// Reset to default
					m_typed = decltype(m_typed)();
					m_it = decltype(m_it)();
					m_end = decltype(m_end)();
				}

				*Base() = *other.Base();
			}

			bool operator!=(const iterator_stack<Depth, N, Ns...>& other) const
			{
				return m_it != other.m_it;
			}

			bool operator==(const iterator_stack<Depth, N, Ns...>& other) const
			{
				return m_it == other.m_it;
			}

			bool AtEnd() const
			{
				return m_it == m_end;
			}

			iterator_stack& operator++()
			{
				assert(!AtEnd());

				++m_it;
				++(*Base());

				return *this;
			}

			template<class K>
			typename std::enable_if<std::is_same<N, K>::value, K&>::type get() const
			{
				return *m_it;
			}

			template<class K>
			typename std::enable_if<!std::is_same<N, K>::value, K&>::type get() const
			{
				return Base()->template get<K>();
			}

			template<std::size_t I>
			auto& get() const
			{
				if constexpr (Depth == I)
					return *m_it;
				else
					return Base()->template get<I>();
			}

		private:
			iterator_stack<Depth + 1, Ns...>* Base()
			{
				return this; // Casts to base
			}

			const iterator_stack<Depth + 1, Ns...>* Base() const
			{
				return this; // Casts to base
			}

			ecscontainerview_t<N> m_typed;
			typename ecscontainerview_t<N>::iterator m_it;
			typename ecscontainerview_t<N>::iterator m_end;
		};

		template <class... Ts>
		using iterator_type = iterator_stack<0, Ts...>;
	};

	template<class... Ts>
	struct Filter
	{
	protected:
		template<class T>
		bool HasChunk(ECSArchetype* pArchetype)
		{
			return pArchetype->GetChunk<T>() != nullptr;
		}
	};

	/// <summary>
	/// AND filter for ECS queries or iteration, valid when all of the given template types are components of an archetype
	/// </summary>
	template<class... Ts>
	struct And : Filter<Ts...>
	{
		bool operator()(ECSArchetype* pArchetype)
		{
			return (this->template HasChunk<Ts>(pArchetype) && ...);
		}
	};

	/// <summary>
	/// NOT filter for ECS queries or iteration, valid when none of the given template types are components of an archetype
	/// </summary>
	template<class... Ts>
	struct Not : Filter<Ts...>
	{
		bool operator()(ECSArchetype* pArchetype)
		{
			return (this->template HasChunk<Ts>(pArchetype) || ...) == false;
		}
	};

	/// <summary>
	/// ANY filter for ECS queries or iteration, valid when any of the given template types are components of an archetype
	/// </summary>
	template<class... Ts>
	struct Any : Filter<Ts...>
	{
		bool operator()(ECSArchetype* pArchetype)
		{
			return (this->template HasChunk<Ts>(pArchetype) || ...);
		}
	};

	/// <summary>
	/// A collection of ECS filters, used to group multiple conditions together
	/// </summary>
	template<class... Filters>
	struct ECSFilterCollection
	{
		bool operator()(ECSArchetype* pArchetype)
		{
			// Verify all filters return true
			return ((Filters()(pArchetype)) && ...);
		}
	};

	template<class Filter, class... Ts>
	class ECSArchetypeQuery
	{
	public:
		class const_iterator
		{
		public:
			const_iterator() 
				: m_archetypes(nullptr) 
				, m_archetypeCurrent(0)
				, m_archetypeCount(0) { }

			// Constructor of the iterator
			// We avoid passing in iterators to the archetype so we can still reallocate it while looping, which is important for queries
			const_iterator(const std::vector<std::unique_ptr<ECSArchetype>>& archetypes, size_t archetypeCurrent)
				: m_archetypes(archetypes)
				, m_archetypeCurrent(archetypeCurrent)
				, m_archetypeCount(archetypes.size())
			{
				// Make sure we begin on an eligable iterator
				FindEligableOrEnd();
			}

			void operator++()
			{
				++m_archetypeCurrent;

				// We increased, make sure it's eligable (or skip ahead)
				FindEligableOrEnd();
			}

			bool operator==(const const_iterator& other) const
			{
				return m_archetypeCurrent == other.m_archetypeCurrent && m_archetypes == other.m_archetypes;
			}

			bool operator!=(const const_iterator& other) const
			{
				return m_archetypeCurrent != other.m_archetypeCurrent || m_archetypes != other.m_archetypes;
			}

			ECSArchetype* operator*() const
			{
				return m_archetypes[m_archetypeCurrent].get();
			}

		private:
			const std::vector<std::unique_ptr<ECSArchetype>>& m_archetypes;
			size_t m_archetypeCurrent;
			size_t m_archetypeCount;

			void FindEligableOrEnd()
			{
				while (m_archetypeCurrent != m_archetypeCount)
				{
					if (IsEligable(**this))
					{
						return;
					}

					++m_archetypeCurrent;
				}
			}

			bool IsEligable(ECSArchetype* pArchetype)
			{
				// First check we have all our required components
				if (!And<Ts...>()(pArchetype))
				{
					// One or more required components were missing, skip archetype
					return false;
				}

				// Then check the rest of the filters, if any
				if (!Filter()(pArchetype))
				{
					return false;
				}

				return true;
			}
		};

		ECSArchetypeQuery()
		{
		}

		ECSArchetypeQuery(const std::vector<std::unique_ptr<ECSArchetype>>& archetypes)
			: m_archetypes(archetypes)
		{
		}

		const_iterator begin() const
		{
			return const_iterator(m_archetypes, 0);
		}

		const_iterator end() const
		{
			return const_iterator(m_archetypes, m_archetypes.size());
		}

	private:
		const std::vector<std::unique_ptr<ECSArchetype>>& m_archetypes;
	};

	template <class Filter, class... Ts>
	class ECSQuery
	{
	public:
		class const_iterator
		{
		public:
			typedef ecscontainerbase_t::size_type difference_type;
			typedef ECSComponentGroup::iterator_type<Ts...> value_type;
			typedef value_type* pointer;
			typedef value_type& reference;
			typedef const value_type& const_reference;
			typedef const value_type* const_pointer;
			typedef std::forward_iterator_tag iterator_category;

			const_iterator() : const_iterator({}, {}) { }

			const_iterator(
				typename ECSArchetypeQuery<Filter, Ts...>::const_iterator&& archetypesItCur,
				typename ECSArchetypeQuery<Filter, Ts...>::const_iterator&& archetypesItEnd)
				: m_archetypeItCur(archetypesItCur)
				, m_archetypeItEnd(archetypesItEnd)
			{ 
				SetNextStack();
			}

			const_iterator& operator++()
			{
				++m_iteratorStack;

				if (m_iteratorStack.AtEnd())
				{
					++m_archetypeItCur;
					SetNextStack();
				}

				return *this;
			}

			bool operator!=(const const_iterator& other) const
			{
				return m_archetypeItCur != other.m_archetypeItCur || m_iteratorStack != other.m_iteratorStack;
			}

			bool operator==(const const_iterator& other) const
			{
				return m_archetypeItCur == other.m_archetypeItCur && m_iteratorStack == other.m_iteratorStack;
			}

			const_reference operator*() const
			{
				assert(!m_iteratorStack.AtEnd());

				return m_iteratorStack;
			}

			const_pointer operator->() const
			{
				assert(!m_iteratorStack.AtEnd());

				return &m_iteratorStack;
			}

			reference operator*()
			{
				assert(!m_iteratorStack.AtEnd());

				return m_iteratorStack;
			}

			pointer operator->()
			{
				assert(!m_iteratorStack.AtEnd());

				return &m_iteratorStack;
			}

		private:
			void SetNextStack()
			{
				while (m_archetypeItCur != m_archetypeItEnd)
				{
					ECSChunk* chunks[sizeof...(Ts)] = { ((*m_archetypeItCur)->template GetChunk<Ts>())... };
					m_iteratorStack = ECSComponentGroup::iterator_type<Ts...>(chunks);

					if (!m_iteratorStack.AtEnd())
					{
						return;
					}

					++m_archetypeItCur;
				}

				m_iteratorStack = ECSComponentGroup::iterator_type<Ts...>(nullptr);
			}

			typename ECSArchetypeQuery<Filter, Ts...>::const_iterator m_archetypeItCur;
			typename ECSArchetypeQuery<Filter, Ts...>::const_iterator m_archetypeItEnd;
			ECSComponentGroup::iterator_type<Ts...> m_iteratorStack;
		};

		ECSQuery(const std::vector<std::unique_ptr<ECSArchetype>>& archetypes)
			: m_archetypeQuery(archetypes)
		{
		}

		const_iterator begin() const
		{
			return const_iterator(m_archetypeQuery.begin(), m_archetypeQuery.end());
		}

		const_iterator end() const
		{
			return const_iterator(m_archetypeQuery.end(), m_archetypeQuery.end());
		}

	private:
		const ECSArchetypeQuery<Filter, Ts...> m_archetypeQuery;
	};

	class ECSManager
	{
	public:
		ECSManager() {}
		~ECSManager() {}

		ECSManager(ECSManager&&) = default;
		ECSManager& operator=(ECSManager&&) = default;

		template<class T>
		static constexpr ECSComponentID ComponentTypeToID(const char* name)
		{
			return ECSComponentID { ECSComponentType<T>::GetHash(), name };
		}

		/// <summary>
		/// Register a named component type
		/// Used internally by the ECS_COMPONENT_* macros
		/// </summary>
		/// <typeparam name="T">Component type</typeparam>
		/// <param name="name">Component name</param>
		/// <returns></returns>
		template<class T>
		static ECSComponentID RegisterComponentType(ECSComponentID id)
		{
			auto& types = GetComponentTypes();
			auto & ptr = types[id.m_hash];
			SLIMECS_ASSERT(ptr == nullptr);

			ptr.reset(new ECSComponentType<T>());

			return id;
		}

		/// <summary>
		/// Get the component metadata attached to a component hash
		/// </summary>
		/// <param name="hash">Hash for component</param>
		/// <returns>Metadata type</returns>
		static IECSComponentType* GetComponentType(ecshash_t hash)
		{
			auto& types = GetComponentTypes();
			auto it = types.find(hash);
			assert(it != types.end()); // All components should be registered statically at init!

			return it != types.end() ? it->second.get() : nullptr;
		}

		/// <summary>
		/// Get or create an archetype for the specified set of components
		/// </summary>
		/// <typeparam name="...Ts">List of components</typeparam>
		/// <returns>Matching archetype</returns>
		template <class... Ts>
		ECSArchetype* GetArchetype()
		{
			constexpr ecshash_t typehash = ECSArchetype::GetComponentsHash<Ts...>();
			ECSArchetype* pArchetype = GetArchetype(typehash);

			if (pArchetype == nullptr)
			{
				const ECSComponentID ids [] = { ECSComponentType<std::remove_cvref_t<Ts>>::m_componentID... };
				pArchetype = CreateArchetype(typehash, ids);
			}

			return pArchetype;
		}

		ECSArchetype* GetArchetype(ECSInstance id)
		{
			KnownECSInstance* known = &m_ids[id.Index()];
			assert(m_ids[id.Index()].m_version == id.Version()); // ECSComponent id version mismatch!

			return m_archetypes[known->m_archetype].get();
		}

		/// <summary>
		/// Create a new instance with the set of components specified for the archetype
		/// </summary>
		/// <param name="pArchetype">Archtetype to construct instance from</param>
		/// <returns>New instance handle</returns>
		ECSInstance CreateInstance(ECSArchetype* pArchetype)
		{
			ECSInstance instance = CreateNewInstance(pArchetype);
			m_ids[instance.Index()].m_idToChunk = pArchetype->CreateInstance(instance.Index());

			return instance;
		}

		/// <summary>
		/// Verify an instance is alive and valid
		/// </summary>
		/// <param name="instance">The instance to check</param>
		/// <returns>True if the instance can be used</returns>
		bool IsInstanceValid(ECSInstance instance) const
		{
			return m_ids[instance.Index()].m_version == instance.Version();
		}

		/// <summary>
		/// Create a new instance with the set of components specified
		/// </summary>
		/// <typeparam name="...Ts">List of components</typeparam>
		/// <returns>New instance handle</returns>
		template<class... Ts>
		ECSInstance CreateInstance()
		{
			return CreateInstance(GetArchetype<Ts...>());
		}

		// Create a new instance with the set of components given
		template<class... Ts>
		ECSInstance CreateInstance(Ts&&... components)
		{
			ECSArchetype* pArchetype = GetArchetype<Ts...>();
			ECSInstance instance = CreateNewInstance(pArchetype);
			m_ids[instance.Index()].m_idToChunk = pArchetype->CreateInstance(instance.Index(), std::forward<Ts>(components)...);

			return instance;
		}

		/// <summary>
		/// Destroy an instance
		/// </summary>
		/// <param name="id">Handle to instance to be destroyed</param>
		void DestroyInstance(ECSInstance id)
		{
			KnownECSInstance* known = &m_ids[id.Index()];
	
			assert(known->m_version == id.Version()); // ECSComponent id version mismatch!

			// Delete instances in chunk
			auto updatedIdsIndex = m_archetypes[known->m_archetype]->DestroyInstance(known->m_idToChunk);

			// This ID was moved to the old instance location
			m_ids[updatedIdsIndex].m_idToChunk = known->m_idToChunk; 

			// Free
			known->m_version++;
			m_freeIds.push_back(id.Index());
		}

		/// <summary>
		/// Destroy all instances that match the given query
		/// </summary>
		template<class... Ts>
		void DestroyInstance()
		{
			DestroyInstance(And<Ts...>());
		}

		/// <summary>
		/// Destroy all instances that match the given query
		/// </summary>
		template<class... Ts, template<class...> class Andable, class... Filterables>
		void DestroyInstance(Andable<Ts...>&&, Filterables&&...)
		{
			const ECSArchetypeQuery<ECSFilterCollection<Filterables...>, Ts...> query(m_archetypes);

			for (ECSArchetype* pArchetype : query)
			{
				ClearArchetype(pArchetype);
			}
		}

		/// <summary>
		/// Add new components to an existing instance
		/// </summary>
		/// <param name="instance">The entity to add components to</param>
		/// <param name="components">One or more components to add to the instance</param>
		template<class... Ts>
		void AddComponent(ECSInstance instance, Ts&&... components)
		{
			// Switch entity to archetype with new components
			AddNewComponents<std::remove_cvref_t<Ts>...>(instance);

			// Add new chunks
			auto pArchetype = GetArchetype(instance);
			(pArchetype->GrowChunkWithComponent(std::forward<Ts>(components)), ...);
		}

		/// <summary>
		/// Remove components from an existing instance
		/// </summary>
		/// <typeparam name="Ts">One or more components to remove</typeparam>
		template<class... Ts>
		void RemoveComponent(ECSInstance instance)
		{
			const ecshash_t removedhashes [] = { ECSComponentType<std::remove_cvref_t<Ts>>::GetHash()... };
			RemoveComponent(instance, removedhashes);
		}

		/// <summary>
		/// Get a reference to one or more components for a single entity, relatively slow. Avoid using this for iteration and use Iterate() instead.
		/// </summary>
		/// <typeparam name="Ts">Component type</typeparam>
		/// <param name="id">Instance handle</param>
		/// <returns>Reference to component data for instance</returns>
		/// <remarks>WARNING: This pointer is volatile and the pointer can invalidate when creation/destruction of entities happens. DO NOT STORE!</remarks>
		template<class... Ts>
		std::tuple<Ts&...> GetComponents(ECSInstance id)
		{
			KnownECSInstance* known = &m_ids[id.Index()];
			assert(m_ids[id.Index()].m_version == id.Version()); // ECSComponent id version mismatch!
			auto& pArchetype = m_archetypes[known->m_archetype];
			auto idToChunk = known->m_idToChunk;

			return std::tuple<Ts&...>(pArchetype->GetChunkView<std::remove_cvref_t<Ts>>()[idToChunk]...);
		}

		/// <summary>
		/// Get a reference to a component for single entity, relatively slow. Avoid using this for iteration and use Iterate() instead.
		/// </summary>
		/// <typeparam name="T">Component type</typeparam>
		/// <param name="id">Instance handle</param>
		/// <returns>Reference to component data for instance</returns>
		/// <remarks>WARNING: This pointer is volatile and the pointer can invalidate when creation/destruction of entities happens. DO NOT STORE!</remarks>
		template<class T>
		T& GetComponent(ECSInstance id)
		{
			return std::get<0>(GetComponents<T>(id));
		}

		/// <summary>
		/// Check if an entity has a specific component type
		/// </summary>
		/// <typeparam name="T">Component type</typeparam>
		/// <param name="id">Instance handle</param>
		/// <returns>True if instance has given component type</returns>
		template<class T>
		bool HasComponent(ECSInstance id)
		{
			auto pArchetype = GetArchetype(id);

			return pArchetype->GetChunk<T>() != nullptr;
		}

		/// <summary>
		/// Iterate over all entities that have at least the set of components specified in the template
		/// </summary>
		/// <typeparam name="Ts">List of components</typeparam>
		/// <example> 
		/// <code>
		/// for (auto&amp; [comp1, comp2] : m_dm.Iterate&lt;TestComponent1, TestComponent2&gt;())
		/// {
		///		comp1 and comp2 can be accessed by reference here
		/// }
		/// </code>
		/// </example>
		template<class... Ts>
		auto Iterate()
		{
			return Iterate(And<Ts...>());
		}

		/// <summary>
		/// Iterate over all entities that match the given filter
		/// </summary>
		/// <example> 
		/// <code>
		/// for (auto&amp; [comp1, comp2] : m_dm.Iterate&lt;And&lt;TestComponent1, TestComponent2&gt;, Not&lt;TestComponent3&gt;&gt;())
		/// {
		///		comp1 and comp2 can be accessed by reference, entities with TestComponent3 will be skipped
		/// }
		/// </code>
		/// </example>
		template<class... Ts, template<class...> class Andable, class... Filterables>
		auto Iterate(Andable<Ts...>&&, Filterables&&...)
		{
			return ECSQuery<ECSFilterCollection<Filterables...>, Ts...>(m_archetypes);
		}

		/// <summary>
		/// Get the count of entities that have at least the given set of components
		/// </summary>
		/// <typeparam name="Ts">One or more components to match</typeparam>
		/// <returns>Count of all matching instances</returns>
		template<class... Ts>
		ecscontainerbase_t::size_type Count()
		{
			return Count(And<Ts...>());
		}

		/// <summary>
		/// Get the count of entities that match the specified filter
		/// </summary>
		template<class... Ts, template<class...> class Andable, class... Filterables>
		auto Count(Andable<Ts...>&&, Filterables&&...)
		{
			const ECSArchetypeQuery<ECSFilterCollection<Filterables...>, Ts...> query(m_archetypes);
			ecscontainerbase_t::size_type count = 0;

			for (const ECSArchetype* pArchetype : query)
			{
				count += pArchetype->Count();
			}

			return count;
		}

	private:
		ECSManager(const ECSManager&) = delete;
		ECSManager& operator=(const ECSManager&) = delete;
		
		// Keeps the relevant indexing data for a single ECSInstace. Fits nicely in 64 bits.
		struct KnownECSInstance
		{
			ecscontainerbase_t::size_type m_idToChunk;
			ecsversion_t m_version;
			ecsarchetype_index_t m_archetype;
		};

		std::vector<std::unique_ptr<ECSArchetype>> m_archetypes;
		std::vector<KnownECSInstance> m_ids;
		std::vector<ecscontainerbase_t::size_type> m_freeIds;

		void ClearArchetype(ECSArchetype* pArchetype)
		{
			for (auto& chunk : pArchetype->m_chunks)
			{
				chunk.Clear();
			}
			
			for (auto index : pArchetype->m_chunkToId)
			{
				m_ids[index].m_version++;
				m_freeIds.push_back(index);
			}

			pArchetype->m_chunkToId.clear();
		}

		void RemoveComponent(ECSInstance instance, const std::span<const ecshash_t>& removedhashes)
		{
			// Straightforward operation:
			// - Reassign entity to an archetype that contains none of the given types
			// - Move each of the existing components over
			// - Done
			auto& known = m_ids[instance.Index()];
			auto pCurrentArchetype = m_archetypes[known.m_archetype].get();
			auto idToCurrentChunk = known.m_idToChunk;

			// Calculate hash for new archetype lookup
			auto newtypehash = pCurrentArchetype->m_typehash;

			for (auto hash : removedhashes)
			{
				assert(pCurrentArchetype->GetChunkByHash(hash) != nullptr); // Verify it exists
				newtypehash = ECSArchetype::CombineTypeHash(newtypehash, hash); // Combine is reversible
			}

			ECSArchetype* pNewArchetype = GetArchetype(newtypehash);

			// If we cannot find the new archetype, we have to construct the full list of components and make it
			if (pNewArchetype == nullptr)
			{
				std::vector<ECSComponentID> allids;
				allids.reserve(pCurrentArchetype->m_chunks.size() - removedhashes.size());

				for (const auto& chunk : pCurrentArchetype->m_chunks)
				{
					bool skip = false;
					for (ecshash_t hash : removedhashes)
					{
						if (hash == chunk.m_componentID.m_hash)
						{
							// Skip
							skip = true;
							break;
						}
					}

					if (skip)
					{
						// This component was removed, so do not add it to the new archetype
						continue;
					}

					allids.push_back(chunk.m_componentID);
				}

				pNewArchetype = CreateArchetype(newtypehash, allids);
			}

			// Assign entity to new archetype
			known.m_archetype = kInvalidArchetypeIndex;
			SetInstanceArchetype(instance, pNewArchetype);

			known.m_idToChunk = pNewArchetype->TrackNewInstance(instance.Index());

			// Move existing components over to new archetype
			for (auto& chunk : pNewArchetype->m_chunks)
			{
				chunk.m_type->GrowMove(pCurrentArchetype->GetChunkByHash(chunk.m_componentID.m_hash), &chunk, idToCurrentChunk);
			}

			// Finally delete the entity from the old archetype
			auto updatedIdsIndex = pCurrentArchetype->DestroyInstance(idToCurrentChunk);

			// If we were the last instance in the archetype, don't reassign ourselves
			if (updatedIdsIndex != instance.Index())
			{
				// This ID was moved to the old instance location
				m_ids[updatedIdsIndex].m_idToChunk = idToCurrentChunk;
			}
		}

		template<class... Ts>
		void AddNewComponents(ECSInstance instance)
		{
			// Straightforward operation:
			// - Reassign entity to an archetype that contains all the new types too
			// - Move each of the existing components over
			// - Done
			auto& known = m_ids[instance.Index()];
			auto pCurrentArchetype = m_archetypes[known.m_archetype].get();
			auto idToCurrentChunk = known.m_idToChunk;

			// Calculate hash for new archetype lookup
			
			auto newtypehash = pCurrentArchetype->m_typehash;
			constexpr ecshash_t hashes [] = { ECSComponentType<std::remove_cvref_t<Ts>>::GetHash()...};

			for (auto componentHash : hashes)
			{
				assert(pCurrentArchetype->GetChunkByHash(componentHash) == nullptr); // Verify it doesn't exist - are you trying to add a component twice?
				newtypehash = ECSArchetype::CombineTypeHash(newtypehash, componentHash);
			}

			ECSArchetype* pNewArchetype = GetArchetype(newtypehash);

			// If we cannot find the new archetype, we have to construct the full list of components and make it
			if (pNewArchetype == nullptr)
			{
				std::vector<ECSComponentID> allids;
				allids.reserve(pCurrentArchetype->m_chunks.size() + sizeof...(Ts));

				for (const auto& chunk : pCurrentArchetype->m_chunks)
				{
					allids.push_back(chunk.m_componentID);
				}

				const ECSComponentID newids [] = { ECSComponentType<std::remove_cvref_t<Ts>>::m_componentID... };
				for (const auto& comp : newids)
				{
					allids.push_back(comp);
				}

				pNewArchetype = CreateArchetype(newtypehash, allids);
			}

			// Assign entity to new archetype
			known.m_archetype = kInvalidArchetypeIndex;
			SetInstanceArchetype(instance, pNewArchetype);

			known.m_idToChunk = pNewArchetype->TrackNewInstance(instance.Index());

			// Move existing components over to new archetype
			for (auto& chunk : pCurrentArchetype->m_chunks)
			{
				chunk.m_type->GrowMove(&chunk, pNewArchetype->GetChunkByHash(chunk.m_componentID.m_hash), idToCurrentChunk);
			}

			// Finally delete the entity from the old archetype
			auto updatedIdsIndex = pCurrentArchetype->DestroyInstance(idToCurrentChunk);

			// If we were the last instance in the archetype, don't reassign ourselves
			if (updatedIdsIndex != instance.Index())
			{
				// This ID was moved to the old instance location
				m_ids[updatedIdsIndex].m_idToChunk = idToCurrentChunk;
			}
		}

		/// <summary>
		/// Create a new instance with the set of components specified for the archetype
		/// </summary>
		/// <param name="pArchetype">Archtetype to construct instance from</param>
		/// <returns>New instance handle</returns>
		ECSInstance CreateNewInstance(ECSArchetype* pArchetype)
		{
			// We create a place for the instance to live here
			// The ECSInstance returned is an indirect id to where it resides
			// ECSInstance.index refers to the index in the manager's index array
			// In the manager's index array, we keep the index to the index inside the archetype's chunk
			// This archetype is kept as part of the index in the manager's index array
			KnownECSInstance* known;

			ecscontainerbase_t::size_type newIndex;
			if (m_freeIds.empty())
			{
				newIndex = m_ids.size();
				known = &m_ids.emplace_back();
				known->m_version = 0;
			}
			else
			{
				known = &m_ids[m_freeIds.back()];
				newIndex = m_freeIds.back();
				m_freeIds.pop_back();
			}

			known->m_archetype = kInvalidArchetypeIndex;
			known->m_version++;

			ECSInstance instance(newIndex, known->m_version, this);
			SetInstanceArchetype(instance, pArchetype);

			return instance;
		}

		/// <summary>
		/// Get or create an archetype for the hash given
		/// </summary>
		/// <returns>Matching archetype</returns>
		ECSArchetype* GetArchetype(ecshash_t typehash)
		{
			// Verify registration
			//int dummy[] = { (RegisterComponentType<Ts>(), 0)... };

			// Check if we don't already have this type
			for (auto& pArchetype : m_archetypes)
			{
				if (pArchetype->m_typehash == typehash)
				{
					// Return cached version
					// We cannot return the fully templated version because the template order matters for some functions
					return pArchetype.get();
				}
			}

			return nullptr;
		}

		/// <summary>
		/// Create an archetype for the given set of components
		/// </summary>
		/// <returns>Matching archetype</returns>
		ECSArchetype* CreateArchetype(ecshash_t hash, const std::span<const ECSComponentID>& components)
		{
			assert(GetArchetype(hash) == nullptr);

			// Create type if we don't have it
			ECSArchetype* pType = m_archetypes.emplace_back(new ECSArchetype(hash, components)).get();

			return pType;
		}

		/// <summary>
		/// Switches an existing instance to a new archetype
		/// </summary>
		/// <param name="instance">The instance to switch</param>
		/// <param name="pArchetype">The archetype to switch to</param>
		void SetInstanceArchetype(ECSInstance instance, ECSArchetype* pArchetype)
		{
			int archetypeIndex = 0;
			for (; archetypeIndex < m_archetypes.size(); archetypeIndex++)
			{
				if (m_archetypes[archetypeIndex].get() == pArchetype)
				{
					break;
				}
			}
	
			assert(archetypeIndex < (int) m_archetypes.size());

			// Return index into first chunk - they should all be identical
			// TODO: Consider letting this return a tuple of chunk references so we can directly init from this function
			auto known = &m_ids[instance.Index()];
			assert(known->m_archetype == kInvalidArchetypeIndex);
			known->m_archetype = archetypeIndex;
		}

		static std::unordered_map<ecshash_t, std::unique_ptr<IECSComponentType>>& GetComponentTypes()
		{
			static std::unordered_map<ecshash_t, std::unique_ptr<IECSComponentType>> types;
			return types;
		}
	};

	/// <summary>
	/// This class allows you to queue up several types of ECS operations
	/// Unlike ECSManager, everything apart from Apply() is threadsafe, allowing you to perform ECS operations from multiple threads at the same time
	/// </summary>
	class ECSMutation
	{
	private:
		class Applier 
		{
		public:
			virtual ~Applier() {}

			virtual void Apply(ECSManager& ecs) = 0;
		};

		typedef std::vector<std::pair<ecshash_t, std::unique_ptr<Applier>>> appliercontainer_t;

		struct OperationData
		{
			appliercontainer_t m_destroyInstance;
			appliercontainer_t m_createInstance;
			appliercontainer_t m_addComponentNoArgs;
			appliercontainer_t m_addComponentWithArgs;
			appliercontainer_t m_removeComponent;
		};

		class CreateInstanceApplier : public Applier
		{
		public:
			void Add(ECSInstance instance)
			{
				m_operations.push_back(instance);
			}

			void Apply(ECSManager& ecs) override
			{
				for (auto& op : m_operations)
				{
					//ecs.CreateInstance(op);
				}

				m_operations.clear();
			}

		private:
			std::vector<ECSInstance> m_operations;
		};

		class DestroyInstanceApplier : public Applier
		{
		public:
			void Add(ECSInstance instance)
			{
				m_operations.push_back(instance);
			}

			void Apply(ECSManager& ecs) override 
			{
				for (auto& op : m_operations)
				{
					ecs.DestroyInstance(op);
				}

				m_operations.clear();
			}

		private:
			std::vector<ECSInstance> m_operations;
		};

		template<class... Ts>
		class AddComponentWithArgsApplier : public Applier
		{
		public:
			void Add(ECSInstance instance, Ts&&... components)
			{
				m_operations.emplace_back(instance, std::forward<Ts>(components)...);
			}

			void Apply(ECSManager& ecs) override
			{
				auto bound = std::bind_front<void(ECSManager::*)(ECSInstance, Ts&&...)>(&ECSManager::AddComponent<Ts...>, &ecs);

				for (auto& op : m_operations)
				{
					std::apply(bound, std::move(op));
				}

				m_operations.clear();
			}

		private:
			std::vector<std::tuple<ECSInstance, Ts...>> m_operations;
		};

		template<class... Ts>
		class RemoveComponentApplier : public Applier
		{
		public:
			void Add(ECSInstance instance)
			{
				m_operations.push_back(instance);
			}

			void Apply(ECSManager& ecs) override
			{
				for (auto& op : m_operations)
				{
					ecs.RemoveComponent<Ts...>(op);
				}

				m_operations.clear();
			}

		private:
			std::vector<ECSInstance> m_operations;
		};

	public:
		ECSInstance CreateInstance() 
		{
			return kInvalidECSInstance;
		}

		void DestroyInstance(ECSInstance instance)
		{
			auto pOperationData = GetOperationsForThread();
			auto pApplier = GetApplier<DestroyInstanceApplier>(pOperationData->m_destroyInstance);
			pApplier->Add(instance);
		}

		template<class... Ts>
		void AddComponent(ECSInstance instance, Ts&&... components)
		{
			auto pOperationData = GetOperationsForThread();
			auto pApplier = GetApplier<AddComponentWithArgsApplier<Ts...>, Ts...>(pOperationData->m_addComponentWithArgs);
			pApplier->Add(instance, std::forward<Ts>(components)...);
		}

		template<class... Ts>
		void RemoveComponent(ECSInstance instance)
		{
			auto pOperationData = GetOperationsForThread();
			auto pApplier = GetApplier<RemoveComponentApplier<Ts...>, Ts...>(pOperationData->m_removeComponent);
			pApplier->Add(instance);
		}

		void Apply(ECSManager& ecs)
		{
			for (auto& op : m_operations)
			{
				Apply(ecs, op.second.get());
			}
		}

	private:
		void Apply(ECSManager& ecs, OperationData* operationData)
		{
			for (auto& a : operationData->m_createInstance)
			{
				a.second->Apply(ecs);
			}

			for (auto& a : operationData->m_addComponentNoArgs)
			{
				a.second->Apply(ecs);
			}

			for (auto& a : operationData->m_addComponentWithArgs)
			{
				a.second->Apply(ecs);
			}

			for (auto& a : operationData->m_removeComponent)
			{
				a.second->Apply(ecs);
			}

			for (auto& a : operationData->m_destroyInstance)
			{
				a.second->Apply(ecs);
			}
		}

		template<class C, class... Ts>
		C* GetApplier(appliercontainer_t& applierVec)
		{
			constexpr ecshash_t hash = ECSArchetype::GetComponentsHash<Ts...>();

			for (auto& a : applierVec)
			{
				if (a.first == hash)
				{
					return static_cast<C*>(a.second.get());
				}
			}

			auto pApplier = new C();
			applierVec.emplace_back(std::make_pair(hash, pApplier));

			return pApplier;
		}

		OperationData* GetOperationsForThread()
		{
			auto tid = std::this_thread::get_id();

			// Try to find thread data
			{
				std::shared_lock lock(m_operationLock);
				auto it = m_operations.find(tid);
				if (it != m_operations.end())
				{
					return it->second.get();
				}
			}

			// Not found
			std::unique_lock lock(m_operationLock);
			auto& operationDataRef = m_operations[tid];
			if (!operationDataRef)
			{
				operationDataRef.reset(new OperationData());
			}
			
			return operationDataRef.get();
		}

		std::shared_mutex m_operationLock;
		std::unordered_map<std::thread::id, std::unique_ptr<OperationData>> m_operations;
	};

	ECSChunk::ECSChunk(const ECSComponentID& componentID)
		: m_componentID(componentID)
	{
		m_type = ECSManager::GetComponentType(componentID.m_hash);
	}

	ECSChunk::~ECSChunk()
	{
		m_type->Clear(this);
	}

	void ECSChunk::Grow()
	{
		m_type->Grow(this);
	}

	void ECSChunk::Remove(ecscontainerbase_t::size_type index)
	{
		m_type->Remove(this, index);
	}

	void ECSChunk::Clear()
	{
		m_type->Clear(this);
	}
}

namespace std
{
	// Template specializations to make ECSComponentGroup::iterator_type tuple-like
	// This allows us to use structured binding on them
	template<typename... Ts> struct tuple_size
		<typename slimecs::ECSComponentGroup::iterator_type<Ts...>>
		: integral_constant<size_t, sizeof...(Ts)> { };

	template<typename T, typename... Ts> struct tuple_element<0, slimecs::ECSComponentGroup::iterator_type<T, Ts...>>
	{
		typedef T type;
	};


	template<std::size_t I, typename T, typename... Ts> struct tuple_element<I, slimecs::ECSComponentGroup::iterator_type<T, Ts...>>
		: tuple_element<I - 1, slimecs::ECSComponentGroup::iterator_type<Ts...>> { };

	// Hash specializations so we can use types as keys for hashing types
	template<>
	struct hash<slimecs::ECSInstance>
	{
		std::size_t operator()(const slimecs::ECSInstance& k) const
		{
			return k;
		}
	};
}