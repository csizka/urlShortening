# URL Shortener

This project contains a containerized URL-Shortening app, 2 optional Databases(Cassandra and Postgres) and their data migration files + tests.

## Running

Run the app with: `docker compose up app` (this will run both databases, but the app will use Cassandra)
It will listen on: http://localhost:8080
To run the tests: `docker compose up test`

## Hashing algorithm

First the URL is parsed (the relative URLS are turned into absolute URLs if possible), and then hashed with base62 MD5 hashing. The last 7 chars of the hash will be the returned handle. this provides 62^7 ~ 3.5e+12 different handles.

## Data scheme 

The handle and the original URL are stored in a table where the handle is the primary key, therefore URLs can be retrieved by indexing the handle they hash to. The timestamp of the insertion is also stored in order to enable future feature expansions such as deletion upon expiration.
  
## Collision handling

When trying to insert a handle - URL pair, if a certain handle is present in the table with a different URL, the handle is hashed again until finding an unoccupied handle. 