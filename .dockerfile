FROM haskell:9.8.4

RUN apt-get update && apt-get install -y libpq-dev g++ libtinfo-dev

WORKDIR /app
COPY . /app

RUN stack setup
RUN stack build

ENV PATH="/root/.local/bin:${PATH}"

CMD ["stack", "exec", "wordlebot-exe"]