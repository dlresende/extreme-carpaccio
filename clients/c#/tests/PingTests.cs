using Microsoft.AspNetCore.Mvc.Testing;
using System.Net;

using Xunit;

namespace XCarpaccio.Tests;

public class PingTests : IClassFixture<WebApplicationFactory<Program>>
{
    private readonly HttpClient _client;

    public PingTests(WebApplicationFactory<Program> factory)
    {
        _client = factory.CreateClient();
    }

    [Fact]
    public async Task PostPingReturnsPong()
    {
        var response = await _client.PostAsync("/ping", content: null);

        Assert.Equal(HttpStatusCode.OK, response.StatusCode);
        Assert.Equal("pong", await response.Content.ReadAsStringAsync());
    }
}
