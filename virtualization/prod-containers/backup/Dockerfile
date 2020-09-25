FROM ubuntu:20.04

####################### Install dependencies #######################

RUN apt-get update && apt-get install -y cron s3cmd ca-certificates

####################### Configure backups #######################

COPY config/crontab /etc/crontab
COPY config/s3cfg /s3cfg_template
COPY scripts/backup-redis.sh /backup-redis.sh
COPY scripts/start.sh /start.sh
RUN chmod 500 /etc/crontab
RUN chmod 400 /s3cfg_template
RUN chmod 500 /backup-redis.sh
RUN chmod 500 /start.sh

####################### Default command #######################

CMD /bin/bash /start.sh
