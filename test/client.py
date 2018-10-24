#!/usr/bin/env python3


import socket
import time

command = b'<stream>'


def run():
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect(("127.0.0.1", 3000))
    print("Socket info: ", s.getsockname())
    print("Connected to server")
    s.send(command)
    print("Sent command: ", command)
    time.sleep(2)
    resp = s.recv(1024)
    print(resp)
    


if __name__ == '__main__':
    run()
