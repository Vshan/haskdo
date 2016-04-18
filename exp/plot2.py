#import plotly
#print plotly.__version__  # version >1.9.4 required
#from plotly.graph_objs import Scatter, Layout
#plotly.offline.plot({
#"data": [
#    Scatter(x=[1, 2, 3, 4], y=[4, 1, 3, 7])
#],
#"layout": Layout(
#    title="hello world"
#)
#})

import plotly
import plotly.graph_objs as go

lines = [line.rstrip('\n') for line in open('../data/plotdata.txt')]
#print lines
nums = lines[0].rsplit(',')[:-1]
print nums
T3 = map(float,nums)
#print T3

sum = 0
for i in T3:
	sum += i

perc = []
for i in T3:
	perc.append((i / sum) * 100)

print perc

fig = {
    'data': [{'labels': ['Study', 'Code', 'Read', 'Write', 'Hygiene', 'Exercise', 'Class', 'ClassWork', 'Food', 'Fun', 'Sleep'],
              'values': perc,
              'type': 'pie'}],
    'layout': {'title': 'Daily Report'}
}

plotly.offline.plot(fig, filename='Pie Chart Example')
