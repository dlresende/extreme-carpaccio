module app;

import vibe.d;

/// HTTP status and body for a request, kept free of vibe.d response types so
/// the routing rules can be tested without starting a server.
struct Reply
{
	int status;
	string body;
}

Reply handleRequest(string method, string path)
{
	if (method == "POST" && path == "/ping")
		return Reply(200, "pong");

	return Reply(404, "Not Found");
}

void handlePing(HTTPServerRequest req, HTTPServerResponse res)
{
	auto reply = handleRequest(req.method, req.path);
	res.statusCode = reply.status;
	res.body = reply.body;
}

void startServer(string address)
{
	auto router = new URLRouter();
	router.post("/ping", &handlePing);

	auto settings = new HTTPServerSettings(address);
	listenHTTP(settings, router);
	runApplication();
}
