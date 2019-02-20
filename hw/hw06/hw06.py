passphrase = '*** PASSPHRASE HERE ***'

def survey(p):
    """
    You do not need to understand this code.
    >>> survey(passphrase)
    '3d2eea56786a3d9e503a4c07dd667867ef3d92bfccd68b2aa0900ead'
    """
    import hashlib
    return hashlib.sha224(p.encode('utf-8')).hexdigest()

class Fib():
    """A Fibonacci number.

    >>> start = Fib()
    >>> start
    0
    >>> start.next()
    1
    >>> start.next().next()
    1
    >>> start.next().next().next()
    2
    >>> start.next().next().next().next()
    3
    >>> start.next().next().next().next().next()
    5
    >>> start.next().next().next().next().next().next()
    8
    >>> start.next().next().next().next().next().next() # Ensure start isn't changed
    8
    """

    def __init__(self, value=0, previous=0):
        self.value = value
        self.previous = previous

    def next(self):
        n = 0

        if self.previous:
            n = self.previous + self.value
        else:
            n = 1

        return Fib(n, self.value)

    def __repr__(self):
        return str(self.value)

class VendingMachine:
    """A vending machine that vends some product for some price.

    >>> v = VendingMachine('candy', 10)
    >>> v.vend()
    'Machine is out of stock.'
    >>> v.deposit(15)
    'Machine is out of stock. Here is your $15.'
    >>> v.restock(2)
    'Current candy stock: 2'
    >>> v.vend()
    'You must deposit $10 more.'
    >>> v.deposit(7)
    'Current balance: $7'
    >>> v.vend()
    'You must deposit $3 more.'
    >>> v.deposit(5)
    'Current balance: $12'
    >>> v.vend()
    'Here is your candy and $2 change.'
    >>> v.deposit(10)
    'Current balance: $10'
    >>> v.vend()
    'Here is your candy.'
    >>> v.deposit(15)
    'Machine is out of stock. Here is your $15.'

    >>> w = VendingMachine('soda', 2)
    >>> w.restock(3)
    'Current soda stock: 3'
    >>> w.restock(3)
    'Current soda stock: 6'
    >>> w.deposit(2)
    'Current balance: $2'
    >>> w.vend()
    'Here is your soda.'
    """
    def __init__(self, item, price):
        self.item = item
        self.price = price
        self.stock_count = 0
        self.amount = 0

    def vend(self):
        if self.stock_count == 0:
            msg = 'Machine is out of stock.'

            if self.amount > 0:
                msg += ' Here is your ${}.'.format(self.amount)

            return msg

        if self.amount < self.price:
            return 'You must deposit ${} more.'.format(self.price - self.amount)

        msg = 'Here is your {}'.format(self.item)

        if self.amount - self.price > 0:
            msg += ' and ${} change'.format(self.amount - self.price)

        msg += '.'

        self.amount = 0
        self.stock_count -= 1

        return msg
        
    def deposit(self, amount):
        if self.stock_count == 0:
            return 'Machine is out of stock. Here is your ${}.'.format(amount)
            
        self.amount += amount
        return 'Current balance: ${}'.format(self.amount)

    def restock(self, count):
        self.stock_count += count
        return 'Current {0} stock: {1}'.format(self.item, self.stock_count)