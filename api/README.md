# API

Minimal API for the public demo.

## Run locally

```
Rscript api/run_api.R
```

## Endpoint

`POST /simulate`

Body (JSON):
```
{
  "player_id": "00-0033873",
  "season": 2024,
  "week": 8,
  "n_sims": 1000,
  "seed": 4242,
  "availability_policy": "played_only",
  "schema_version": "v1"
}
```

If `player_id` is missing, the API will attempt to resolve `player_name` against
the cached player directory.
