module app_test;

import app;

unittest
{
	auto reply = handleRequest("POST", "/ping");
	assert(reply.status == 200);
	assert(reply.body == "pong");
}
