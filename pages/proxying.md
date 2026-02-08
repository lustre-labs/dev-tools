# API Proxying

Often you'll be developing your Lustre application alongside a separate backend
app. This can cause some problems during development as you'll typically run into
CORS issues when your Lustre app tries to make requests to your backend running
on a different port.

To solve this, Lustre's development server includes support for proxying API
requests to another server. This means your application code doesn't require
environment-specific logic to determine where to send API requests to, instead
it always hits a path like `/api/users` and the development server takes care of
proxying that request to your backend.

## Configuration

To configure the API proxy, you'll need to add a `proxy` entry to your `gleam.toml`
file under the `tools.lustre.dev` section. The most common configuration looks 
something like this:

```toml
[tools.lustre.dev]
proxy = { from = "/api", to = "http://localhost:3000/api" }
```

In this example, any requests made to paths starting with `/api` will be forwarded
to `http://localhost:3000/api`. So a request to `/api/users` would be proxied to
`http://localhost:3000/api/users`.

You can also proxy multiple paths to different endpoints in your backend, or even
different servers if you need to: 

```toml
[tools.lustre.dev]
proxy = [{ from = "/api", to = "http://localhost:3000/api" }, { from = "/auth", to = "http://localhost:3000/auth" }]
```

In this example, requests will be forwarded to the `auth` or `api` endpoints depending 
on the request's path. :wa
