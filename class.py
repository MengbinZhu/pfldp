class Human(object):
    laugh="hahahahaha"
    def show_laugh(self):
        print self.laugh
    def laugh_100(self):
        for i in range(100):
            print i
            self.show_laugh()

Hanmeimei = Human()
Hanmeimei.laugh_100()
