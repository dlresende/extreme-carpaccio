module app;

import vibe.d;

/// HTTP status and body for a request, so the routing rules can be exercised
/// without starting a server.
struct Reply
{
	int status;
	string body;
}

Reply handleRequest(HTTPMethod method, string path)
{
	if (method == HTTPMethod.POST && path == "/ping")
		return Reply(200, "pong");

	return Reply(404, "Not Found");
}

void handlePing(HTTPServerRequest req, HTTPServerResponse res)
{
	auto reply = handleRequest(req.method, req.path);
	res.statusCode = reply.status;
	res.writeBody(reply.body, "text/plain; charset=utf-8");
}

void startServer(string address)
{
	auto router = new URLRouter();
	router.post("/ping", &handlePing);

	auto settings = new HTTPServerSettings(address);
	listenHTTP(settings, router);
	runApplication();
}
