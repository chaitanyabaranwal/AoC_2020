from collections import Counter

class PolicyPassword:
    def __init__(self, string):
        policy = string.split(': ')[0]
        self.password = string.split(': ')[1]
        self.letter = policy.split()[1]
        self.min = int(policy.split()[0].split('-')[0])
        self.max = int(policy.split()[0].split('-')[1])

    def is_valid(self):
        count = Counter(self.password)
        if self.min <= count[self.letter] <= self.max:
            return True
        return False

class PolicyPassword2:
    def __init__(self, string):
        policy = string.split(': ')[0]
        self.password = string.split(': ')[1]
        self.letter = policy.split()[1]
        self.index_1 = int(policy.split()[0].split('-')[0]) - 1
        self.index_2 = int(policy.split()[0].split('-')[1]) - 1

    def is_valid(self):
        count_1 = 1 if self.password[self.index_1] == self.letter else 0
        count_2 = 1 if self.password[self.index_2] == self.letter else 0
        return count_1 + count_2 == 1

def main():
    count = 0
    with open('2.txt') as f:
        lines = f.readlines()
        for line in lines:
            policy_password = PolicyPassword2(line)
            if policy_password.is_valid():
                count += 1
    print(count)

main()
