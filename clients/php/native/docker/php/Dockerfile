FROM php:fpm

WORKDIR /home/project

RUN apt-get update && apt-get upgrade -y && apt-get install -y git libssl-dev wget && \
    docker-php-ext-install zip mbstring && \
	pecl install xdebug-beta && docker-php-ext-enable xdebug

RUN curl -sS https://getcomposer.org/installer | php -- --install-dir=/usr/local/bin --filename=composer
