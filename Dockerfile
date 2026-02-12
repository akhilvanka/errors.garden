FROM haskell:9.6-slim AS build
WORKDIR /app
RUN apt-get update && apt-get install -y libgmp-dev && rm -rf /var/lib/apt/lists/*
COPY stack.yaml stack.yaml.lock package.yaml ./
RUN stack build --only-dependencies --system-ghc
COPY src ./src
RUN stack build --system-ghc && stack install --system-ghc --local-bin-path /app/bin

FROM debian:bookworm-slim
RUN apt-get update && apt-get install -y libgmp10 ca-certificates && rm -rf /var/lib/apt/lists/*
COPY --from=build /app/bin/errors-garden /usr/local/bin/
EXPOSE 8080
CMD ["errors-garden"]
