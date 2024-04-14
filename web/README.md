# README

Training Blocks is a fitness platform for runners.

## Setup

```
bundle install
yarn
```

Add to `.env` file and run `set -a && . ./.env && set +a`
```
STRAVA_SECRET=xxx
RAILS_SERVE_STATIC_FILES=true
ROLLBAR_ACCESS_TOKEN=xxxx
ROLLBAR_CLIENT_ACCESS_TOKEN=xxxx
MAILGUN_API_KEY=xxx
```

## Develop

```
yarn dev
```

## Test

```
rspec
```

Front-end:
```
yarn test
```
