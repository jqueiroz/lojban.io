FROM redis

COPY config/redis.conf /redis.conf
RUN chown redis /redis.conf
CMD chown -R redis /database && redis-server /redis.conf
