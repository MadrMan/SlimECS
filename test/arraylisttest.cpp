#include "../include/slimecs/arraylist.h"
#include "gtest/gtest.h"

#include <string>

namespace slimecs
{
	struct MyTestStruct
	{
		int n;
		std::string s;
	};

	TEST(ArraylistTests, SingleElementTest)
	{
		MyTestStruct s{ 1337, "hi" };

		arraylist<MyTestStruct> arr;
		auto& sref = arr.emplace_back(s);

		EXPECT_EQ(s.n, sref.n);
		EXPECT_EQ(s.s, sref.s);

		auto& sindexref = arr[0];
		EXPECT_EQ(s.n, sindexref.n);
		EXPECT_EQ(s.s, sindexref.s);

		arr.pop_back();
	}

	TEST(ArraylistTests, IteratorTest)
	{
		const int kMaxCount = 1234;
		const std::string kText = "hi";

		arraylist<MyTestStruct, 5> arr;

		for (int x = 0; x < kMaxCount; x++)
		{
			arr.emplace_back(MyTestStruct { x, kText });
		}

		int counter = 0;
		for (auto& elem : arr)
		{
			EXPECT_EQ(elem.n, counter);
			EXPECT_EQ(elem.s, kText);

			counter++;
		}
	}

	TEST(ArraylistTests, CanPushAnySize)
	{
		arraylist<int, 123> arr;

		for (int x = 1; x < 1000; x++)
		{
			arr.clear();
			for (int y = 0; y < x; y++)
			{
				arr.emplace_back(y);
			}
		}
	}

	TEST(ArraylistTests, CanIterateAnySize)
	{
		arraylist<int, 123> arr;

		for (int x = 1; x < 1000; x++)
		{
			arr.clear();
			for (int y = 0; y < x; y++)
			{
				arr.emplace_back(y);

				int z = 0;
				for (int n : arr)
				{
					z++;
				}

				EXPECT_GT(z, 0);
			}
		}
	}

	TEST(ArraylistTests, CanPopAnySize)
	{
		arraylist<int, 123> arr;

		for (int x = 1; x < 1000; x++)
		{
			for (int y = 0; y < x; y++)
			{
				arr.emplace_back(y);
			}

			while (!arr.empty())
			{
				arr.pop_back();
			}
		}
	}
}
