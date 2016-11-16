package org.baconpaul.pianoroll;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GradientPaint;

import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.*;
import org.jfree.data.xy.*;
import org.jfree.ui.ApplicationFrame;
import org.jfree.ui.RefineryUtilities;
import org.jfree.chart.renderer.xy.XYErrorRenderer;

public class PianoRoll extends javax.swing.JFrame
{
    public PianoRoll( String title, XYIntervalSeriesCollection boxes )
    {
	super( title );
	final JFreeChart chart = createChart( boxes );
	final ChartPanel chartPanel = new ChartPanel( chart );
	chartPanel.setPreferredSize( new Dimension( 900, 500 ));
	setContentPane( chartPanel );
    }

     private JFreeChart createChart(final XYIntervalSeriesCollection dataset) {
	NumberAxis xAxis = new NumberAxis("Time");
	NumberAxis yAxis = new NumberAxis("Note");
	XYErrorRenderer rend = new BoxRenderer();
	rend.setBaseLinesVisible( false );
	rend.setBaseShapesVisible( true );

	XYPlot p = new XYPlot( dataset, xAxis, yAxis, rend);
	    
	JFreeChart chart = new JFreeChart( "Piano Roll", p);
	
        return chart;
    }

    public static void main(final String[] args) {
	BoxSeries s = new BoxSeries( "Random Data" );
	s.add( 10, 9, 11, 23, 21, 24, 0.2);
	//s.add( 17, 14, 18, 21, 18, 25, 0.4);

	BoxSeries s2 = new BoxSeries( "More Random Data" );
	s2.add( 17, 14, 18, 21, 18, 25, 0.99);
	

	XYIntervalSeriesCollection res = new XYIntervalSeriesCollection();
	res.addSeries( s );
	res.addSeries( s2 );
	
        final PianoRoll demo = new PianoRoll( "Demo", res );
        demo.pack();
        RefineryUtilities.centerFrameOnScreen(demo);
        demo.setVisible(true);

    }


}
