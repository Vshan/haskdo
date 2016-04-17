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

fig = {
    'data': [{'labels': ['Residential', 'Non-Residential', 'Utility'],
              'values': [19, 26, 55],
              'type': 'pie'}],
    'layout': {'title': 'Daily Report'}
}

plotly.offline.plot(fig, filename='Pie Chart Example')
