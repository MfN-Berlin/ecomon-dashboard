# Setup Hasura configuration
hasura_url = Sys.getenv("HASURA_URL")
hasura_headers = c(
  "Content-Type" = "application/json",
  "x-hasura-admin-secret" = Sys.getenv("HASURA_SECRET")
)
