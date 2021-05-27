
#include <extreme_carpaccio_client/ExtremeCarpaccioClient.hpp>

#include <vector>
#include <numeric>

namespace kata {
namespace bowling_game {

const size_t TOTAL_PINS = 10;
const size_t TOTAL_FRAMES = 10;

static bool isSpare(const std::vector<int> & rolls, size_t frameIndex)
{
   return rolls[frameIndex] + rolls[frameIndex + 1] == TOTAL_PINS;
}

static bool isStrike(const std::vector<int>& rolls, size_t frameIndex)
{
   return rolls[frameIndex] == TOTAL_PINS;
}

int bowlingScore(const std::vector<int> & rolls)
{
   int score = 0;
   size_t frameIndex = 0;

   for(size_t frame = 0; frame < TOTAL_FRAMES; ++frame)
   {
      if(isStrike(rolls, frameIndex))
      {
         score += TOTAL_PINS + rolls[frameIndex+1] + rolls[frameIndex+2];
         frameIndex++;
      }
      else if (isSpare(rolls, frameIndex))
      {
         score += TOTAL_PINS + rolls[frameIndex+2];
         frameIndex += 2;
      }
      else
      {
         score += rolls[frameIndex] + rolls[frameIndex + 1];
         frameIndex += 2;
      }
   }

   return score;
}

} // namespace bowling_game
} // namespace kata
