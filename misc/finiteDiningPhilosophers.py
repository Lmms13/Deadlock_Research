import threading
import time

class Philosopher(threading.Thread):
    def __init__(self, name, left_fork, right_fork):
        threading.Thread.__init__(self)
        self.name = name
        self.left_fork = left_fork
        self.right_fork = right_fork

    def run(self):
        print(f"{self.name} is thinking.")
        time.sleep(1)
        print(f"{self.name} is hungry.")
        self.dine()

    def dine(self):
        fork1, fork2 = self.left_fork, self.right_fork

        while True:
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
        time.sleep(1)
        print(f"{self.name} finishes eating and leaves to think.")

if __name__ == "__main__":
    forks = [threading.Lock() for _ in range(10)]
    philosophers = [
        Philosopher(f"Philosopher {i}", forks[i % 10], forks[(i + 1) % 10])
        for i in range(10)
    ]

    for philosopher in philosophers:
        philosopher.start()

    for philosopher in philosophers:
        philosopher.join()