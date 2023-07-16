#include "gtest/gtest.h"

#include <memory>
#include <iostream>
#include "../include/slimecs/slimecs.h"

struct TestComponent1
{
	uint32_t mem1;
};

SLIMECS_COMPONENT_DEFINE(TestComponent1);

struct TestComponent2
{
	uint32_t mem2;
};

SLIMECS_COMPONENT_DEFINE(TestComponent2);

struct TestComponent3
{
	float mem3;
};

SLIMECS_COMPONENT_DEFINE(TestComponent3);

struct LeakComponent
{
	int* m_pVar = nullptr;

	LeakComponent() { }
	~LeakComponent()
	{
		*m_pVar = 0;
	}
};

SLIMECS_COMPONENT_DEFINE(LeakComponent);

struct OperatorTrackingComponent
{
	bool wasDefaultConstructed = false;
	bool wasCopyConstructed = false;
	bool wasMoveConstructed = false;
	bool wasMoveAssigned = false;
	bool wasCopyAssigned = false;
	int value = 0;

	OperatorTrackingComponent()
	{ 
		wasDefaultConstructed = true; 
	}

	OperatorTrackingComponent(const OperatorTrackingComponent& o) 
	{ 
		wasCopyConstructed = true;
		value = o.value;
	}

	OperatorTrackingComponent(OperatorTrackingComponent&& o) noexcept
	{ 
		wasMoveConstructed = true; 
		value = o.value;
		o.value = 0;
	}

	~OperatorTrackingComponent() {}

	OperatorTrackingComponent& operator=(const OperatorTrackingComponent& o) 
	{ 
		wasCopyAssigned = true;
		value = o.value;
		return *this;
	}

	OperatorTrackingComponent& operator=(OperatorTrackingComponent&& o) noexcept
	{
		wasMoveAssigned = true;
		value = o.value;
		o.value = 0;
		return *this;
	}
};

SLIMECS_COMPONENT_DEFINE(OperatorTrackingComponent);

namespace slimecs
{
	class ECSTest : public ::testing::Test
	{
	public:
		ECSManager m_dm;
	};

	TEST_F(ECSTest, ArchetypesAreCreated)
	{
		auto pArche1 = m_dm.GetArchetype<TestComponent1, TestComponent2, TestComponent3>();
		auto pArche3 = m_dm.GetArchetype<TestComponent2, TestComponent3>();

		ASSERT_NE(nullptr, pArche1);
		ASSERT_NE(nullptr, pArche3);
	}

	TEST_F(ECSTest, EntitiesAreValid)
	{
		auto id1 = m_dm.CreateInstance(TestComponent1());
		auto id2 = m_dm.CreateInstance(TestComponent1());

		ASSERT_TRUE(id1.IsValid());
		ASSERT_TRUE(id2.IsValid());
	}

	TEST_F(ECSTest, EntitiesCanBeCounted)
	{
		auto id1 = m_dm.CreateInstance(TestComponent1());
		auto id2 = m_dm.CreateInstance(TestComponent1());

		ASSERT_EQ(m_dm.Count<TestComponent1>(), 2);

		m_dm.DestroyInstance(id1);
		ASSERT_EQ(m_dm.Count<TestComponent1>(), 1);

		m_dm.DestroyInstance(id2);
		ASSERT_EQ(m_dm.Count<TestComponent1>(), 0);
	}

	TEST_F(ECSTest, EntitiesCanBeDestroyedWithFilter)
	{
		m_dm.CreateInstance(TestComponent1());
		m_dm.CreateInstance(TestComponent1());
		m_dm.CreateInstance(TestComponent2());
		m_dm.CreateInstance(TestComponent1(), TestComponent2());

		ASSERT_EQ(m_dm.Count<TestComponent2>(), 2);

		m_dm.DestroyInstance<TestComponent1>();

		ASSERT_EQ(m_dm.Count<TestComponent2>(), 1);
		ASSERT_EQ(m_dm.Count<TestComponent1>(), 0);
	}

	TEST_F(ECSTest, EntityFromArchetypeHasComponent)
	{
		auto pArche1 = m_dm.GetArchetype<TestComponent1, TestComponent2>();

		ECSInstance id1 = m_dm.CreateInstance(pArche1);
		ASSERT_TRUE(m_dm.HasComponent<TestComponent1>(id1));
		ASSERT_TRUE(m_dm.HasComponent<TestComponent2>(id1));
		ASSERT_FALSE(m_dm.HasComponent<TestComponent3>(id1));

		m_dm.DestroyInstance(id1);
	}

	TEST_F(ECSTest, EntityFromComponentsHasComponent)
	{
		ECSInstance id1 = m_dm.CreateInstance(TestComponent1(), TestComponent2());
		ASSERT_TRUE(m_dm.HasComponent<TestComponent1>(id1));
		ASSERT_TRUE(m_dm.HasComponent<TestComponent2>(id1));
		ASSERT_FALSE(m_dm.HasComponent<TestComponent3>(id1));

		m_dm.DestroyInstance(id1);
	}

	TEST_F(ECSTest, EntityComponentCreateCanCopyConstruct)
	{
		OperatorTrackingComponent comp;
		comp.value = 123;

		ECSInstance id = m_dm.CreateInstance(comp);
		auto& compref = m_dm.GetComponent<OperatorTrackingComponent>(id);

		ASSERT_EQ(compref.value, 123);
		ASSERT_FALSE(compref.wasDefaultConstructed);
		ASSERT_TRUE(compref.wasCopyConstructed);
		ASSERT_FALSE(compref.wasMoveConstructed);
		ASSERT_FALSE(compref.wasCopyAssigned);
		ASSERT_FALSE(compref.wasMoveAssigned);
	}

	TEST_F(ECSTest, EntityComponentCreateCanMoveConstruct)
	{
		OperatorTrackingComponent comp;
		comp.value = 123;

		ECSInstance id = m_dm.CreateInstance(std::move(comp));
		auto& compref = m_dm.GetComponent<OperatorTrackingComponent>(id);

		ASSERT_EQ(compref.value, 123);
		ASSERT_FALSE(compref.wasDefaultConstructed);
		ASSERT_FALSE(compref.wasCopyConstructed);
		ASSERT_TRUE(compref.wasMoveConstructed);
		ASSERT_FALSE(compref.wasCopyAssigned);
		ASSERT_FALSE(compref.wasMoveAssigned);
	}

	TEST_F(ECSTest, EntityComponentAddCanCopyConstruct)
	{
		OperatorTrackingComponent comp;
		comp.value = 123;

		ECSInstance id = m_dm.CreateInstance(TestComponent1{});
		m_dm.AddComponent(id, comp);
		auto& compref = m_dm.GetComponent<OperatorTrackingComponent>(id);

		ASSERT_EQ(compref.value, 123);
		ASSERT_FALSE(compref.wasDefaultConstructed);
		ASSERT_TRUE(compref.wasCopyConstructed);
		ASSERT_FALSE(compref.wasMoveConstructed);
		ASSERT_FALSE(compref.wasCopyAssigned);
		ASSERT_FALSE(compref.wasMoveAssigned);
	}

	TEST_F(ECSTest, EntityComponentAddCanMoveConstruct)
	{
		OperatorTrackingComponent comp;
		comp.value = 123;

		ECSInstance id = m_dm.CreateInstance(TestComponent1{});
		m_dm.AddComponent(id, std::move(comp));
		auto& compref = m_dm.GetComponent<OperatorTrackingComponent>(id);

		ASSERT_EQ(compref.value, 123);
		ASSERT_FALSE(compref.wasDefaultConstructed);
		ASSERT_FALSE(compref.wasCopyConstructed);
		ASSERT_TRUE(compref.wasMoveConstructed);
		ASSERT_FALSE(compref.wasCopyAssigned);
		ASSERT_FALSE(compref.wasMoveAssigned);
	}

	TEST_F(ECSTest, GetComponentIsMutable)
	{
		auto pArche1 = m_dm.GetArchetype<TestComponent1>();
		ECSInstance id1 = m_dm.CreateInstance(pArche1);
		ECSInstance id2 = m_dm.CreateInstance(pArche1);
		ECSInstance id3 = m_dm.CreateInstance(pArche1);

		m_dm.GetComponent<TestComponent1>(id1).mem1 = 111u;
		m_dm.GetComponent<TestComponent1>(id2).mem1 = 222u;
		m_dm.GetComponent<TestComponent1>(id3).mem1 = 333u;

		m_dm.DestroyInstance(id1);
		m_dm.DestroyInstance(id3);

		ASSERT_EQ(222u, m_dm.GetComponent<TestComponent1>(id2).mem1);
	}

	TEST_F(ECSTest, Iteration)
	{
		auto pArche1 = m_dm.GetArchetype<TestComponent1, TestComponent2, TestComponent3>();
		auto pArche3 = m_dm.GetArchetype<TestComponent2, TestComponent3>();

		ECSInstance id1 = m_dm.CreateInstance(pArche1);
		m_dm.GetComponent<TestComponent1>(id1).mem1 = 1;
		m_dm.GetComponent<TestComponent2>(id1).mem2 = 2;
		m_dm.GetComponent<TestComponent3>(id1).mem3 = 3;

		ECSInstance id2 = m_dm.CreateInstance<TestComponent1>();
		m_dm.GetComponent<TestComponent1>(id2).mem1 = 1;

		ECSInstance id3 = m_dm.CreateInstance(pArche3);
		m_dm.GetComponent<TestComponent2>(id3).mem2 = 2;
		m_dm.GetComponent<TestComponent3>(id3).mem3 = 3;

		int counter = 0;

		{
			auto query = m_dm.Iterate<TestComponent2, TestComponent3>();
			auto end = query.end();

			for (auto it = query.begin(); it != end; ++it)
			{
				auto& [comp2, comp3] = *it;
				std::cout << "Comp2: " << comp2.mem2 << " Comp3: " << comp3.mem3 << std::endl;

				ASSERT_EQ(2, comp2.mem2);
				ASSERT_EQ(3, comp3.mem3);

				counter++;
			};
		}

		counter = 0;

		for (auto& [comp2, comp3] : m_dm.Iterate<TestComponent2, TestComponent3>())
		{
			std::cout << "Comp2: " << comp2.mem2 << " Comp3: " << comp3.mem3 << std::endl;

			ASSERT_EQ(2, comp2.mem2);
			ASSERT_EQ(3, comp3.mem3);

			counter++;
		};

		ASSERT_EQ(2, counter);
	}

	TEST_F(ECSTest, QueryIteratorIsWellDefined)
	{
		m_dm.CreateInstance(TestComponent1{ 0 });
		{
			auto query1 = m_dm.Iterate<TestComponent1>();
			ASSERT_EQ(std::distance(query1.begin(), query1.end()), 1);
		}

		m_dm.CreateInstance(TestComponent1{ 1 });
		{
			auto query2 = m_dm.Iterate<TestComponent1>();
			ASSERT_EQ(std::distance(query2.begin(), query2.end()), 2);
		}

		m_dm.CreateInstance(TestComponent1{ 2 });
		{
			auto query3 = m_dm.Iterate<TestComponent1>();
			ASSERT_EQ(std::distance(query3.begin(), query3.end()), 3);
		}

		m_dm.CreateInstance(TestComponent1{ 3 });
		{
			auto query4 = m_dm.Iterate<TestComponent1>();
			ASSERT_EQ(std::distance(query4.begin(), query4.end()), 4);
		}

		auto query = m_dm.Iterate<TestComponent1>();
		auto it = query.begin();
		auto end = query.end();
		{
			ASSERT_EQ(std::distance(it, end), 4);
		}

		{
			auto it2 = it;
			ASSERT_EQ(std::distance(it2, end), 4);
			ASSERT_NE(it2, end);
			ASSERT_EQ(it2->get<0>().mem1, 0);

			ASSERT_EQ(std::distance(++it2, end), 3);
			ASSERT_NE(it2, end);
			ASSERT_EQ(it2->get<0>().mem1, 1);

			ASSERT_EQ(std::distance(++it2, end), 2);
			ASSERT_NE(it2, end);
			ASSERT_EQ(it2->get<0>().mem1, 2);

			ASSERT_EQ(std::distance(++it2, end), 1);
			ASSERT_NE(it2, end);
			ASSERT_EQ(it2->get<0>().mem1, 3);

			ASSERT_EQ(std::distance(++it2, end), 0);
			ASSERT_EQ(it2, end);
		}

		for (auto it = query.begin(); it != end; ++it)
		{
			auto& [comp] = *it;
		};
	}

	TEST_F(ECSTest, IterationCanExclude)
	{
		auto pArche1 = m_dm.GetArchetype<TestComponent1, TestComponent2, TestComponent3>();
		auto pArche3 = m_dm.GetArchetype<TestComponent2, TestComponent3>();

		ECSInstance id1 = m_dm.CreateInstance(pArche1);
		m_dm.GetComponent<TestComponent1>(id1).mem1 = 1;
		m_dm.GetComponent<TestComponent2>(id1).mem2 = 1;
		m_dm.GetComponent<TestComponent3>(id1).mem3 = 1;

		ECSInstance id2 = m_dm.CreateInstance<TestComponent1>();
		m_dm.GetComponent<TestComponent1>(id2).mem1 = 2;

		ECSInstance id3 = m_dm.CreateInstance(pArche3);
		m_dm.GetComponent<TestComponent2>(id3).mem2 = 3;
		m_dm.GetComponent<TestComponent3>(id3).mem3 = 3;

		ECSInstance id4 = m_dm.CreateInstance<TestComponent2>();
		m_dm.GetComponent<TestComponent2>(id4).mem2 = 4;

		ECSInstance id5 = m_dm.CreateInstance<TestComponent3>();
		m_dm.GetComponent<TestComponent3>(id5).mem3 = 5;

		int counter = 0;

		for (auto& [comp1] : m_dm.Iterate(And<TestComponent1>(), Not<TestComponent3>()))
		{
			std::cout << "Comp1: " << comp1.mem1 << std::endl;

			ASSERT_EQ(2, comp1.mem1);

			counter++;
		};

		ASSERT_EQ(1, counter);
	}

	TEST_F(ECSTest, ComponentsAreMutable)
	{
		ECSInstance id1 = m_dm.CreateInstance(TestComponent1 {
			123
		});

		for (auto& [comp1] : m_dm.Iterate<TestComponent1>())
		{
			ASSERT_EQ(123, comp1.mem1);

			comp1.mem1 = 456;
		};

		ASSERT_EQ(456, m_dm.GetComponent<TestComponent1>(id1).mem1);
	}

	TEST_F(ECSTest, ManyEntitiesAreStable)
	{
		int counter1 = 0;
		int counter2 = 0;
		const int instanceCount = 100000;
		const int loopCount = 10;

		std::vector<ECSInstance> instances;
		instances.reserve(instanceCount);

		for (int x = 0; x < instanceCount; x++)
		{
			TestComponent1 c1; c1.mem1 = x;
			TestComponent2 c2; c2.mem2 = x * 2;

			instances.push_back(m_dm.CreateInstance(c1, c2));

			counter1 += x * loopCount;
			counter2 += x * 2 * loopCount;
		}

		for (int y = 0; y < loopCount; y++)
		{
			auto query = m_dm.Iterate<TestComponent1, TestComponent2>();
			auto end = query.end();

			for (auto& [comp1, comp2] : m_dm.Iterate<TestComponent1, TestComponent2>())
			{
				counter1 -= comp1.mem1;
				counter2 -= comp2.mem2;
			};
		}

		ASSERT_EQ(m_dm.Count<TestComponent1>(), instanceCount);

		for (int x = 0; x < instanceCount; x++)
		{
			m_dm.DestroyInstance(instances[x]);
		}

		ASSERT_EQ(m_dm.Count<TestComponent1>(), 0);

		ASSERT_EQ(0, counter1);
		ASSERT_EQ(0, counter2);
	}
	TEST_F(ECSTest, ComponentDestructorFires)
	{
		int var = 1;
		{
			ECSManager m_dm2;

			auto inst = m_dm2.CreateInstance<LeakComponent>();
			auto& pComp = m_dm2.GetComponent<LeakComponent>(inst);

			pComp.m_pVar = &var;
			EXPECT_EQ(var, 1);
		}

		// We expect the scope close to kill the ECSManager, firing off the destructor
		EXPECT_EQ(var, 0);
	}

	TEST_F(ECSTest, ComponentAdded)
	{
		auto inst = m_dm.CreateInstance<TestComponent1>();
		m_dm.GetComponent<TestComponent1>(inst).mem1 = 1;
		EXPECT_TRUE(m_dm.HasComponent<TestComponent1>(inst));
		EXPECT_FALSE(m_dm.HasComponent<TestComponent2>(inst));
		EXPECT_FALSE(m_dm.HasComponent<TestComponent3>(inst));

		m_dm.AddComponent(inst, TestComponent2{ 2 });
		EXPECT_TRUE(m_dm.HasComponent<TestComponent1>(inst));
		EXPECT_TRUE(m_dm.HasComponent<TestComponent2>(inst));
		EXPECT_FALSE(m_dm.HasComponent<TestComponent3>(inst));

		m_dm.AddComponent(inst, TestComponent3{ 3 });
		EXPECT_TRUE(m_dm.HasComponent<TestComponent1>(inst));
		EXPECT_TRUE(m_dm.HasComponent<TestComponent2>(inst));
		EXPECT_TRUE(m_dm.HasComponent<TestComponent3>(inst));

		EXPECT_EQ(m_dm.GetComponent<TestComponent1>(inst).mem1, 1);
		EXPECT_EQ(m_dm.GetComponent<TestComponent2>(inst).mem2, 2);
		EXPECT_EQ(m_dm.GetComponent<TestComponent3>(inst).mem3, 3);
	}

	TEST_F(ECSTest, ComponentRemoved)
	{
		auto inst = m_dm.CreateInstance<TestComponent1, TestComponent2, TestComponent3>();
		m_dm.GetComponent<TestComponent1>(inst).mem1 = 1;
		m_dm.GetComponent<TestComponent2>(inst).mem2 = 1;
		m_dm.GetComponent<TestComponent3>(inst).mem3 = 1;
		EXPECT_TRUE(m_dm.HasComponent<TestComponent1>(inst));
		EXPECT_TRUE(m_dm.HasComponent<TestComponent2>(inst));
		EXPECT_TRUE(m_dm.HasComponent<TestComponent3>(inst));

		m_dm.RemoveComponent<TestComponent3>(inst);
		EXPECT_TRUE(m_dm.HasComponent<TestComponent1>(inst));
		EXPECT_TRUE(m_dm.HasComponent<TestComponent2>(inst));
		EXPECT_FALSE(m_dm.HasComponent<TestComponent3>(inst));

		m_dm.RemoveComponent<TestComponent2>(inst);
		EXPECT_TRUE(m_dm.HasComponent<TestComponent1>(inst));
		EXPECT_FALSE(m_dm.HasComponent<TestComponent2>(inst));
		EXPECT_FALSE(m_dm.HasComponent<TestComponent3>(inst));

		EXPECT_EQ(m_dm.GetComponent<TestComponent1>(inst).mem1, 1);
	}

	TEST_F(ECSTest, ComponentsTouchedWithMutator)
	{
		auto inst = m_dm.CreateInstance<TestComponent1>();
		ECSMutation mut;

		mut.AddComponent<TestComponent2>(inst);
		mut.RemoveComponent<TestComponent1>(inst);

		mut.Apply(m_dm);

		EXPECT_FALSE(m_dm.HasComponent<TestComponent1>(inst));
		EXPECT_TRUE(m_dm.HasComponent<TestComponent2>(inst));
		EXPECT_FALSE(m_dm.HasComponent<TestComponent3>(inst));

		mut.AddComponent(inst, TestComponent1{ 1 }, TestComponent3{ 3 });
		mut.RemoveComponent<TestComponent2>(inst);

		mut.Apply(m_dm);

		EXPECT_TRUE(m_dm.HasComponent<TestComponent1>(inst));
		EXPECT_FALSE(m_dm.HasComponent<TestComponent2>(inst));
		EXPECT_TRUE(m_dm.HasComponent<TestComponent3>(inst));
	}
}