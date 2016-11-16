package org.baconpaul.pianoroll;

import java.awt.Graphics2D;
import java.awt.Paint;
import java.awt.Stroke;
import java.awt.geom.Line2D;
import java.awt.geom.Rectangle2D;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;

import org.jfree.chart.axis.ValueAxis;
import org.jfree.chart.event.RendererChangeEvent;
import org.jfree.chart.plot.CrosshairState;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.chart.plot.PlotRenderingInfo;
import org.jfree.chart.plot.XYPlot;
import org.jfree.data.Range;
import org.jfree.data.xy.IntervalXYDataset;
import org.jfree.data.xy.XYDataset;
import org.jfree.io.SerialUtilities;
import org.jfree.ui.RectangleEdge;
import org.jfree.util.ObjectUtilities;
import org.jfree.util.PaintUtilities;

import org.jfree.chart.renderer.xy.*;
import org.jfree.chart.renderer.*;
import org.jfree.data.xy.*;
import java.awt.*;
import java.awt.geom.*;


public class BoxRenderer extends org.jfree.chart.renderer.xy.XYErrorRenderer
{
    @Override
    public void drawItem(Graphics2D g2, XYItemRendererState state,
			 Rectangle2D dataArea, PlotRenderingInfo info, XYPlot plot,
			 ValueAxis domainAxis, ValueAxis rangeAxis, XYDataset dataset,
			 int series, int item, CrosshairState crosshairState, int pass) {
        if (pass == 0 && dataset instanceof IntervalXYDataset
	    && getItemVisible(series, item)) {

            IntervalXYDataset ixyd = (IntervalXYDataset) dataset;
            PlotOrientation orientation = plot.getOrientation();

	    // draw the error bar for the x-interval
	    double x0 = ixyd.getStartXValue(series, item);
	    double x1 = ixyd.getEndXValue(series, item);

	    RectangleEdge edge = plot.getDomainAxisEdge();
	    double xxx0 = domainAxis.valueToJava2D(x0, dataArea, edge);
	    double xxx1 = domainAxis.valueToJava2D(x1, dataArea, edge);
	    double xx0 = Math.min( xxx0, xxx1 );
	    double xx1 = Math.max( xxx0, xxx1 );


	    double y0 = ixyd.getStartYValue(series, item);
	    double y1 = ixyd.getEndYValue(series, item);

	    edge = plot.getRangeAxisEdge();
	    double yyy0 = rangeAxis.valueToJava2D(y0, dataArea, edge);
	    double yyy1 = rangeAxis.valueToJava2D(y1, dataArea, edge);
	    double yy0 = Math.min( yyy0, yyy1 );
	    double yy1 = Math.max( yyy0, yyy1 );

	    double pct = 1.0;
	    if( dataset instanceof XYIntervalSeriesCollection &&
		((XYIntervalSeriesCollection)dataset).getSeries( series ) instanceof BoxSeries )
		{
		    pct = ((BoxSeries)((XYIntervalSeriesCollection)dataset).getSeries(series)).getPct( item );
		}
	    
	    Rectangle2D box;
	    if (orientation == PlotOrientation.VERTICAL) {
		box = new Rectangle2D.Double( xx0, yy0, (xx1-xx0), (yy1-yy0));
	    }
	    else {  // PlotOrientation.HORIZONTAL
		box = new Rectangle2D.Double( yy0, xx0, (yy1-yy0), (xx1-xx0));
	    }

	    java.awt.Color fill = Color.white;
	    if( getItemPaint( series, item ) instanceof java.awt.Color ) {
		fill = (Color)getItemPaint( series, item );
		fill = new java.awt.Color( (int)(fill.getRed() * pct),
					   (int)(fill.getGreen() * pct),
					   (int)(fill.getBlue() * pct) );
	    }
	    g2.setPaint( fill );
	    g2.fill( box );
	    
	    if (getErrorPaint() != null) {
		g2.setPaint(getErrorPaint());
	    }
	    else {
		g2.setPaint(getItemPaint(series, item));
	    }
	    if (getErrorStroke() != null) {
		g2.setStroke(getErrorStroke());
	    }
	    else {
		g2.setStroke(getItemStroke(series, item));
	    }
	    g2.draw( box );

	}
	// I want to draw over the default
	//super.drawItem(g2, state, dataArea, info, plot, domainAxis, rangeAxis,
	//dataset, series, item, crosshairState, pass);

    }
}
