# File System MCP Server By Clojure

* Inspiration: https://github.com/bhauman/clojure-mcp

## Run in emacs 

```
(mcp-server)
```
![](./demo3.png)

## Use it by Claude Desktop

```json
{
  "clojure-mcp": {
    "command": "/bin/bash",
    "args": [
      "-c",
      "cd /Users/clojure/Desktop/datascript-clj-mcp.git && /usr/local/bin/clojure -M -m clojure-mcp.core"
    ]
  }
}
```
![](./demo1.png)
![](./demo2.png)
