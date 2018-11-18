FROM ubuntu:14.10

# docker build -t lambda .
# docker run --name lambda --tty --network net -d lambda 
# docker stop lambda
# docker start lambda


EXPOSE 8080

ADD . /opt/
WORKDIR /opt/
ENTRYPOINT /opt/game-of-life-exe
