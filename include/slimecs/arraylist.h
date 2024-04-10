#pragma once

#include <vector>
#include <memory>
#include <cassert>
#include <type_traits>

namespace slimecs
{
	class arraylist_data
	{
	public:
		typedef unsigned char slice_type;
		typedef std::vector<std::unique_ptr<slice_type[]>> container_t;
		typedef typename container_t::size_type size_type;

		arraylist_data()
			: m_elements(0) { }

		~arraylist_data()
		{
			// Do not ever rely on arraylist_data to clean up the slices, the destructors won't be called!
			assert(m_slices.empty());
		}

		arraylist_data(arraylist_data&& other) noexcept
			: m_elements(other.m_elements)
			, m_slices(std::move(other.m_slices))
		{
			other.m_elements = 0;
		}

		template<class T>
		T* get_slice(size_type index) const
		{
			return reinterpret_cast<T*>(m_slices[index].get());
		}

		template<class T, int ELEMENT_COUNT>
		T* add_slice()
		{
			auto& ptr = m_slices.emplace_back(new slice_type[sizeof(T) * ELEMENT_COUNT]);
			return reinterpret_cast<T*>(ptr.get());
		}

		void pop_slice()
		{
			m_slices.pop_back();
		}

		size_type m_elements;
		container_t m_slices;

	private:
		arraylist_data(const arraylist_data& other) = delete;
		arraylist_data& operator=(const arraylist_data& other) = delete;
		arraylist_data& operator=(arraylist_data&& other) = delete;
	};

	template<typename T, int SLICE_SIZE = 1024>
	class arraylist_view
	{
	public:
		typedef arraylist_data::size_type size_type;
		typedef T value_type;
		typedef value_type& reference;
		typedef value_type* pointer;
		typedef const value_type* const_reference;
		typedef const value_type* const_pointer;

		class const_iterator;
		class iterator;

		typedef iterator iterator_type;
		typedef const_iterator const_iterator_type;

		arraylist_view()
			: m_data(nullptr) { }

		arraylist_view(arraylist_data& data)
			: m_data(&data) { }

		virtual ~arraylist_view() { }

		arraylist_view(const arraylist_view& other)
			: m_data(other.m_data)
		{
		}

		bool has_data() const
		{
			return m_data != nullptr;
		}

		arraylist_view& operator=(const arraylist_view& other)
		{
			m_data = other.m_data;

			return *this;
		}

		template<typename... Args>
		reference emplace_back(Args&&... args)
		{
			size_type indexInSlice, indexOfSlice;
			index_to_slice(m_data->m_elements, &indexInSlice, &indexOfSlice);

			pointer slice;
			if (m_data->m_slices.size() <= indexOfSlice)
			{
				slice = m_data->add_slice<T, SLICE_SIZE>();
			}
			else
			{
				slice = m_data->get_slice<T>(indexOfSlice);
			}

			pointer pElem = new (&slice[indexInSlice]) T(std::forward<Args>(args)...);
			m_data->m_elements++;

			return *pElem;
		}

		void clear()
		{
			for (reference elem : *this)
			{
				elem.~T();
			}

			m_data->m_slices.clear();
			m_data->m_elements = 0;
		}

		void pop_back()
		{
			(*this)[m_data->m_elements - 1].~T();
			m_data->m_elements--;

			if (m_data->m_elements % SLICE_SIZE == 0)
			{
				// We just emptied a slice
				m_data->pop_slice();
			}
		}

		reference operator[](size_t index)
		{
			assert(index < m_data->m_elements);

			size_type indexInSlice, indexOfSlice;
			index_to_slice(index, &indexInSlice, &indexOfSlice);

			return m_data->get_slice<T>(indexOfSlice)[indexInSlice];
		}

		bool empty()
		{
			return m_data->m_elements == 0;
		}

		reference back()
		{
			assert(!empty());

			return (*this)[m_data->m_elements - 1];
		}

		pointer erase(size_type index)
		{
			return erase(&(*this)[index]);
		}

		pointer erase(pointer elem)
		{
			auto* pback = &back();
			if (elem == pback)
			{
				// Erasing last elem, so just pop
				pop_back();
				return nullptr;
			}

			// Destroy elem and move last element to newly freed position
			elem->~T();
			new (elem) T(std::move(*pback));
			pop_back();

			return elem;
		}

		size_type size() const
		{
			return m_data->m_elements;
		}

		class const_iterator
		{
		public:
			typedef typename arraylist_view::size_type difference_type;
			typedef typename arraylist_view::value_type value_type;
			typedef typename arraylist_view::const_pointer pointer;
			typedef typename arraylist_view::const_reference reference;
			typedef typename arraylist_view::const_reference const_reference;
			typedef std::random_access_iterator_tag iterator_category;

			const_iterator()
				: m_data(nullptr), m_index(std::numeric_limits<decltype(m_index)>::max()), m_iterator(nullptr) { }

			const_iterator(const arraylist_data* data, size_type index)
				: m_data(data), m_index(index)
			{
				size_type indexInSlice, indexOfSlice;
				arraylist_view::index_to_slice(index, &indexInSlice, &indexOfSlice);

				if (m_data->m_slices.empty())
				{
					m_iterator = nullptr;
					assert(m_index == 0);
				}
				else
				{
					set_iterator_to_slice(indexOfSlice);
					m_iterator += indexInSlice;
				}
			}

			const_iterator& operator++()
			{
				m_index++;

				if (m_index % SLICE_SIZE == 0)
				{
					size_type indexOfSlice = m_index / SLICE_SIZE;
					set_iterator_to_slice(indexOfSlice);
				}
				else
				{
					m_iterator++;
				}

				return *this;
			}

			difference_type operator-(const_iterator other) const
			{
				return m_index - other.m_index;
			}

			const_iterator operator++(int)
			{
				const_iterator temp(*this); ++(*this); return temp;
			}

			const_iterator operator+(difference_type offset) const
			{
				return const_iterator(m_data, m_index + offset);
			}

			reference operator*() const { return *m_iterator; }
			pointer operator->() const { return m_iterator; }

			bool operator == (const const_iterator& other) const 
			{ 
				assert(m_data == other.m_data || m_data == nullptr || other.m_data == nullptr);

				return m_index == other.m_index; 
			}
			bool operator != (const const_iterator& other) const 
			{ 
				assert(m_data == other.m_data || m_data == nullptr || other.m_data == nullptr);

				return m_index != other.m_index;
			}

		protected:
			void set_iterator_to_slice(size_type indexOfSlice)
			{
				if (indexOfSlice >= m_data->m_slices.size())
				{
					m_iterator = nullptr;
				}
				else
				{
					m_iterator = m_data->get_slice<T>(indexOfSlice);
				}
			}

			const arraylist_data* m_data;
			pointer m_iterator;
			size_type m_index;

		private:
		};

		class iterator : public const_iterator
		{
		public:
			typedef typename arraylist_view::pointer pointer;
			typedef typename arraylist_view::reference reference;

			iterator() { }

			iterator(const arraylist_data* data, size_type index)
				: const_iterator(data, index)
			{
			}

			iterator& operator++()
			{
				++base();

				return *this;
			}

			iterator operator+(typename const_iterator::difference_type offset) const
			{
				return iterator(this->m_data, this->m_index + offset);
			}

			iterator operator++(int) { iterator temp(*this); ++(*this); return temp; }
			reference operator*() const { return *const_cast<pointer>(this->m_iterator); }
			pointer operator->() const { return const_cast<pointer>(this->m_iterator); }

		private:
			const_iterator& base() 
			{
				return *this;
			}
		};

		iterator begin()
		{
			return iterator(m_data, 0);
		}

		iterator end()
		{
			return iterator(m_data, m_data->m_elements);
		}

		const_iterator begin() const
		{
			return const_iterator(m_data, 0);
		}

		const_iterator end() const
		{
			return const_iterator(m_data, m_data->m_elements);
		}

	private:
		static void index_to_slice(size_type index, size_type* indexInSlice, size_type* indexOfSlice)
		{
			*indexInSlice = index % SLICE_SIZE;
			*indexOfSlice = index / SLICE_SIZE;
		}

		arraylist_data* m_data;
	};

	// Core functionality:
	// - Grow
	// - Shrink
	// - Iterate
	// - Get by index
	// Characteristics:
	// - Almost literally a deque, but with a templateless base
	// - Configurable chunk size
	// - Chunks of the "deque" are guaranteed to be continuous
	template<typename T, int SLICE_SIZE = 1024>
	class arraylist : public arraylist_view<T, SLICE_SIZE>
	{
	public:
		arraylist() 
			: arraylist_view<T, SLICE_SIZE>(m_data) { }

		arraylist(arraylist&& other)
			: arraylist_view<T, SLICE_SIZE>(m_data)
			, m_data(std::move(other.m_data)) { }

		virtual ~arraylist() 
		{
			this->clear();
		}

	private:
		arraylist(const arraylist& other) = delete;
		arraylist& operator=(const arraylist& other) = delete;
		arraylist& operator=(arraylist&& other) = delete;

		arraylist_data m_data;
	};
}
