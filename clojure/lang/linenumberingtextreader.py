class PushbackTextReader():
    def __init__(self, reader):
        self.baseReader = reader
        self.unreadChar = 0
        self.hasUnread = False
