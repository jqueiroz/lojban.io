FROM ubuntu:20.04

####################### Install dependencies #######################

RUN apt-get update && apt-get install -y cron s3cmd ca-certificates

####################### Configure backups #######################

COPY config/crontab /etc/crontab
COPY config/s3cfg /s3cfg
COPY scripts/backup-redis.sh /backup-redis.sh
RUN chmod 500 /etc/crontab
RUN chmod 400 /s3cfg
RUN chmod 500 /backup-redis.sh

####################### Default command #######################

CMD cron -f
#CMD /bin/bash /backup-redis.sh
