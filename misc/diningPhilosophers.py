import threading
import signal
import sys
import time

#test

class Philosopher(threading.Thread):
    def __init__(self, name, left_fork, right_fork, stop_event):
        threading.Thread.__init__(self)
        self.name = name
        self.left_fork = left_fork
        self.right_fork = right_fork
        self.stop_event = stop_event

    def run(self):
        while not self.stop_event.is_set():
            print(f"{self.name} is thinking.")
            self.sleep_interruptible(1)
            print(f"{self.name} is hungry.")
            self.dine()

    def dine(self):
        fork1, fork2 = self.left_fork, self.right_fork

        while not self.stop_event.is_set():
            fork1.acquire(True)
            locked = fork2.acquire(False)
            if locked:
                break
            fork1.release()
            print(f"{self.name} swaps forks.")
            fork1, fork2 = fork2, fork1

        try:
            self.eat()
        finally:
            if fork2.locked():
                fork2.release()
            if fork1.locked():
                fork1.release()

    def eat(self):
        print(f"{self.name} starts eating.")
        self.sleep_interruptible(1)
        print(f"{self.name} finishes eating and leaves to think.")

    def sleep_interruptible(self, duration):
        self.stop_event.wait(duration)

def signal_handler(sig, frame):
    print("Stopping philosophers...")
    stop_event.set()
    for philosopher in philosophers:
        philosopher.join()
    sys.exit(0)

if __name__ == "__main__":
    stop_event = threading.Event()
    signal.signal(signal.SIGINT, signal_handler)

    forks = [threading.Lock() for _ in range(10)]
    philosophers = [
        Philosopher(f"Philosopher {i}", forks[i % 10], forks[(i + 1) % 10], stop_event)
        for i in range(10)
    ]

    for philosopher in philosophers:
        philosopher.start()

    try:
        while not stop_event.is_set():
            time.sleep(1)
    except KeyboardInterrupt:
        signal_handler(signal.SIGINT, None)