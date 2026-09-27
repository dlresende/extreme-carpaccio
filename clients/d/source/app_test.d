module app_test;

import vibe.d : HTTPMethod;

import app;

unittest
{
	auto reply = handleRequest(HTTPMethod.post, "/ping");
	assert(reply.status == 200);
	assert(reply.body == "pong");
}
