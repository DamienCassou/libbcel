# basecamp-proxy

This tools connects to the Basecamp 3 API and opens a TCP port so
another tool can connect to it. Communication with the proxy is done
with JSON.

## Usage

Start the proxy:

```session
$ ./bin/basecamp-proxy 2222 23483270
Basecamp proxy listening on 2222
```

The first number is the TCP port to open. The second number is the
account id: this is the first number appearing after basecamp.com in
the URL when you are on the basecamp website.

While this is running, open another shell and start a TCP connection:

```session
$ telnet localhost 2222
Trying ::1...
Connected to localhost.
Escape character is '^]'.
```

You then have to give access to your Basecamp account. This is done
through OAUTH. If you already have a client id and secret, skip the next section. Otherwise, keep reading.

### Obtaining a client id and secret

Login to https://basecamp.com/ and register an
application at https://launchpad.37signals.com/integrations. You can
use whatever values you want but you should at least:

- check "Basecamp 3" in the "Products" section;
- set a redirect URI as "http://localhost:9321". You can use a
  different port but it must differ from the TCP port passed to the
  proxy above (i.e., "2222").

You'll be assigned a client id and secret. Save that somewhere so you
can skip this step next time.

### Authenticating

You have a client id and secret. You are now ready to send your first
query to the proxy. Go back to the telnet session opened above and
type the following:

```session
{"id":1, "type":"meta", "action":"generate_auth_token", "client_id":"xxxxxxxxxx", "client_secret":"yyyyyyyyyy", "port":9321}
```

Replace the client id and secret with the values obtained above. The
port specified here is the one saved as redirect URI above (9321 being
the default, you don't have to specify it).

This should open a new tab in your favorite web browser. Authorize the
access and you should now get an answer in the telnet session:

```session
{"id":1,"type":"success","payload":{}}
```

This indicates that the proxy is now successfully connected to
Basecamp 3 API. Congratulations.

### Querying Basecamp

To get a list of all Basecamp projects, type this in the telnet session:

```session
{"id":2, "type":"projects","action":"find"}
```

You can pass any value for the `id` property and the proxy will send
the value back in the response. This can be used to match responses to
queries if several queries are sent in parallel.

As response to this query, the proxy will send a payload containing an
array of projects.
