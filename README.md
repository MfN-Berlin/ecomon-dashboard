# ecomon-dashboard

## Development

You need some data for testing, e.g. for a site, species year, and minimum threshold.

Make sure the correct URL is set in the .env file.
For local development, that would be the address of the local service as seen from Docker:
`HASURA_URL=172.17.0.1:8080/v1/graphql`

Start the ecomon dev environment from the ecomon project.

Start the dashboard using docker compose in ecomon-dashboard

Go to localhost:3000 and open the dashboard page.
