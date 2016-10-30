import matplotlib.pyplot as plt
import numpy as np

class plot_ss:
    """ plot_ss(x, y)
        Interactive scatter plot of x,y data. User defines fit line.
    """
    def __init__(self, x, y, showSquares=False):
        self.datax = x
        self.datay = y
        self.showsq = showSquares
        
        self.linex = []
        self.liney = []
        
        # y = m x + b
        self.m = None
        self.b = None
        #self.m = 0.77 
        #self.b = 5032 
        
        
        self.fig = plt.figure()
        self.g = self.fig.add_subplot(111)
        self.g.scatter(x, y)
        self.xlim = self.g.get_xlim()
        self.ylim = self.g.get_ylim()
        self.g.set_title("Click two points to make a line")
        self.fig.canvas.mpl_connect('button_press_event', self.on_press)
        #self.residual()

    def on_press(self, event):
        if len(self.linex) < 2:
            x, y = event.xdata, event.ydata
            self.g.plot(x, y, 'r*', scalex=False, scaley=False)
            self.linex.append(x)
            self.liney.append(y)
           
            if len(self.linex)==2:
                xvals = np.linspace(*self.g.get_xlim())
                yvals = self.fit_line(xvals)
                self.g.plot(xvals, yvals, "r", scalex=False, scaley=False)
                res2 = self.residual()
                title = "line: y = {:n} x + {:n}; sum of squares = {:n}"
                self.g.set_title(title.format(self.m, self.b, res2))
                
        return 

    def fit_value(self, x):
        if self.m is None:
            self.m = (self.liney[1] - self.liney[0])/(self.linex[1] - self.linex[0])
            self.b = (self.liney[0]*self.linex[1] - self.linex[0]*self.liney[1])/(self.linex[1] - self.linex[0])
            print(self.m, self.b)
        return self.m * x + self.b
    
    def fit_line(self, xvals):
        print("fit_line")
        return [self.fit_value(x) for x in xvals]        
    
    def residual(self):
        res2 = 0
        for x,y in zip(self.datax, self.datay):
            yhat = self.fit_value(x)
            res2 += pow(y-yhat, 2)
            self.g.vlines(x, min(y, yhat), max(y, yhat), linestyle="--", color="c")
            if self.showsq:
                self.squares(x, y, yhat)
        self.g.set_xlim(*self.xlim)
        self.g.set_ylim(*self.ylim)
        return res2
    
    def squares(self, x, y, yhat):
        res = y - yhat
        x1 = x - res
        self.g.vlines(x1, min(y, yhat), max(y, yhat), linestyle=":", color="r")
        self.g.hlines(y, min(x, x1), max(x, x1), linestyle=":", color="r")
        self.g.hlines(yhat, min(x, x1), max(x, x1), linestyle=":", color="r")
        return
