module app_test;

import std.exception : assert;

import app;

unittest
{
	auto reply = handleRequest("POST", "/ping");
	assert(reply.status == 200);
	assert(reply.body == "pong");
}
