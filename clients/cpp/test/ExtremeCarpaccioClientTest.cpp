
#include <extreme_carpaccio_client/ExtremeCarpaccioClient.hpp>

#include <gtest/gtest.h>

#include <vector>

using namespace kata::bowling_game;

TEST(BowlingGame, acceptance_test)
{
   EXPECT_EQ(133, bowlingScore(std::vector<int>{1, 4, 4, 5, 6, 4, 5, 5, 10, 0, 1, 7, 3, 6, 4, 10, 2, 8, 6}));
}

TEST(BowlingGame, one_pin_at_first)
{
   EXPECT_EQ(1, bowlingScore(std::vector<int>{1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, gutter_game)
{
   EXPECT_EQ(0, bowlingScore(std::vector<int>{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, several_pins_at_first)
{
   EXPECT_EQ(6, bowlingScore(std::vector<int>{6, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, several_pins_at_second_roll)
{
   EXPECT_EQ(5, bowlingScore(std::vector<int>{0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, several_pins_at_first_2_rolls)
{
   EXPECT_EQ(8, bowlingScore(std::vector<int>{3, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, several_pins_at_last_rolls)
{
   EXPECT_EQ(7, bowlingScore(std::vector<int>{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 7}));
}

TEST(BowlingGame, several_pins_at_first_3_rolls)
{
   EXPECT_EQ(9, bowlingScore(std::vector<int>{1, 3, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, 3_pins_at_all_rolls)
{
   EXPECT_EQ(60, bowlingScore(std::vector<int>{3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3}));
}

TEST(BowlingGame, one_spare_at_beginning)
{
   EXPECT_EQ(18, bowlingScore(std::vector<int>{6, 4, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, another_spare_at_beginning)
{
   EXPECT_EQ(16, bowlingScore(std::vector<int>{5, 5, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, spare_at_frame_2)
{
   EXPECT_EQ(14, bowlingScore(std::vector<int>{0, 0, 3, 7, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, spare_at_frame_3)
{
   EXPECT_EQ(24, bowlingScore(std::vector<int>{0, 0, 0, 0, 7, 3, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, spare_at_last_frame)
{
   EXPECT_EQ(13, bowlingScore(std::vector<int>{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 6, 4, 3}));
}

TEST(BowlingGame, 5_pins_at_all_rolls)
{
   EXPECT_EQ(150, bowlingScore(std::vector<int>{5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5, 5}));
}

TEST(BowlingGame, fake_spare_between_first_2_frames)
{
   EXPECT_EQ(11, bowlingScore(std::vector<int>{0, 5, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, one_strike_at_beginning)
{
   EXPECT_EQ(20, bowlingScore(std::vector<int>{10, 4, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, another_strike_at_beginning)
{
   EXPECT_EQ(18, bowlingScore(std::vector<int>{10, 2, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, one_strike_at_frame_2)
{
   EXPECT_EQ(14, bowlingScore(std::vector<int>{0, 0, 10, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}));
}

TEST(BowlingGame, one_strike_at_last_frame)
{
   EXPECT_EQ(17, bowlingScore(std::vector<int>{0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 10, 3, 4}));
}

TEST(BowlingGame, perfect_game)
{
   EXPECT_EQ(300, bowlingScore(std::vector<int>(12, 10)));
}
