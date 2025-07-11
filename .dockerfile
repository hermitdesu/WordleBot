FROM haskell:9.2.8

RUN apt-get update && apt-get install -y libpq-dev g++ libtinfo-dev

WORKDIR /app
COPY . /app

RUN stack setup
RUN stack build --only-dependencies
RUN stack build --copy-bins

ENV PATH="/root/.local/bin:${PATH}"

CMD ["stack", "exec", "wordlebot-exe"]