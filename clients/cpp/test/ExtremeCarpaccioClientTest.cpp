
#include <extreme_carpaccio_client/ExtremeCarpaccioClient.hpp>

#include <gtest/gtest.h>

#include <vector>

using namespace extreme_carpaccio_client;

TEST(ExtremeCarpaccioClient, should_handle_feedback)
{
   EXPECT_EQ(1, 1);
}

TEST(ExtremeCarpaccioClient, should_handle_order)
{
   EXPECT_EQ(1, 1);
}

//it('should handle feedback', function(done) {
//   var feedback = request('http://' + conf.host + ':' + conf.port + '/feedback', function(error, response) {
//      expect(response.statusCode).toBe(204);
//      done();
//   });
//   feedback.write('{type: "INFO", content: "this is my precious feedback"}');
//});
//
//it('should handle order', function(done) {
//   var order = request('http://' + conf.host + ':' + conf.port + '/order', function(error, response, body) {
//      expect(response.statusCode).toBe(200);
//      expect(body).toBe('{total:0}');
//      done();
//   });
//   order.write('{prices: [], quantities: [], country: "DE", reduction: "STANDARD"}');
//});

