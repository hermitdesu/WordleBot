FROM haskell:9.8.4

# Установка необходимых библиотек
RUN apt-get update && \
    apt-get install -y g++ libpq-dev libtinfo6 && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Кэширование зависимостей
COPY stack.yaml stack.yaml.lock package.yaml /app/
RUN stack setup && stack build --only-dependencies

# Копируем весь проект
COPY . /app

# Сборка проекта и копирование бинарника
RUN stack build --copy-bins

# Добавляем путь к бинарнику
ENV PATH="/root/.local/bin:${PATH}"

# Запускаем бинарник напрямую (без stack exec)
CMD ["wordlebot-exe"]
