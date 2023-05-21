FROM node as build-node

WORKDIR /app
COPY ./static/public ./public
COPY ./static/src ./src
COPY ./static/package.json ./package.json
COPY ./static/package-lock.json ./package-lock.json
RUN npm install && npm run build

FROM rust as build-rust
WORKDIR /app
COPY ./src ./src
COPY ./Cargo.lock ./Cargo.lock
COPY ./Cargo.toml ./Cargo.toml
RUN cargo build --release

FROM gcr.io/distroless/cc

WORKDIR /app
COPY --from=build-rust /app/target/release/musicbrainz ./
COPY --from=build-node /app/build ./static/build
COPY ./Rocket.toml ./

EXPOSE 8000
CMD ["./musicbrainz"]