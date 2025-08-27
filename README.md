# AITA

AITA is an OCaml project built with [Dune](https://dune.build/) and hosted using the [Dream](https://aantron.github.io/dream/) web framework.

This project integrates:

- **OpenRouter** (chat completions)
- **Mopsa**

## Configuration

Environment variables:

- `OPENROUTER_API_KEY`: Your OpenRouter API key
- `OPENROUTER_MODEL` (optional): Model id (default: `anthropic/claude-3.5-sonnet`)

Run the server:

```bash
opam switch mopsa
eval $(opam env)
dune exec bin/main.exe -- -v
```
