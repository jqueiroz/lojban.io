FROM ubuntu:16.04

####################### Install dependencies #######################

RUN apt-get update && apt-get install -y nginx

####################### Configure nginx #######################

RUN echo "daemon off;" >> /etc/nginx/nginx.conf
RUN rm /etc/nginx/sites-enabled/default
COPY static /static/
COPY config/nginx.conf /etc/nginx/sites-enabled/master-https.conf
RUN chown www-data -R /static

####################### Expose ports #######################

EXPOSE 443

####################### Default command #######################

CMD nginx
