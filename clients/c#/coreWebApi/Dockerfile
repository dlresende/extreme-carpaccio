FROM microsoft/dotnet:runtime

RUN mkdir -p /usr/src/app
WORKDIR /usr/src/app
COPY src/xcarpaccio/bin/Debug/netcoreapp1.1/publish .
ENTRYPOINT ["dotnet", "xcarpaccio.dll","5000"]

EXPOSE 5000