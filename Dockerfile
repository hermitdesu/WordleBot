FROM haskell:9.8.4

# Устанавливаем необходимые системные библиотеки (g++, libtinfo6 и пр.)
RUN apt-get update && \
    apt-get install -y g++ libpq-dev libtinfo6 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY . /app

# Настраиваем GHC (скачиваем, если нет)
RUN stack setup

# Собираем зависимости (только зависимости)
RUN stack build --only-dependencies

# Собираем бинарник и копируем его в ~/.local/bin
RUN stack build --copy-bins

ENV PATH="/root/.local/bin:${PATH}"

# Запускаем приложение
CMD ["stack", "exec", "wordlebot-exe"]
