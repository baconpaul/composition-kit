package org.baconpaul.pianoroll;

import java.awt.Color;
import java.awt.Dimension;
import java.awt.GradientPaint;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.axis.NumberAxis;
import org.jfree.chart.plot.*;
import org.jfree.data.xy.*;
import org.jfree.ui.ApplicationFrame;
import org.jfree.ui.RefineryUtilities;
import org.jfree.chart.renderer.xy.XYErrorRenderer;

/**
 * A simple demonstration application showing how to create a bar chart.
 *
 */
public class SampleViz extends ApplicationFrame {

    /**
     * Creates a new demo instance.
     *
     * @param title  the frame title.
     */
    public SampleViz(final String title) {

        super(title);

        final XYIntervalSeriesCollection dataset = createDataset();
        final JFreeChart chart = createChart(dataset);
        final ChartPanel chartPanel = new ChartPanel(chart);
        chartPanel.setPreferredSize(new Dimension(900, 470));
        setContentPane(chartPanel);

    }

    /**
     * Returns a sample dataset.
     * 
     * @return The dataset.
     */
    private XYIntervalSeriesCollection createDataset() {
	BoxSeries s = new BoxSeries( "Random Data" );
	s.add( 10, 9, 11, 23, 21, 24, 0.2);
	//s.add( 17, 14, 18, 21, 18, 25, 0.4);

	BoxSeries s2 = new BoxSeries( "More Random Data" );
	s2.add( 17, 14, 18, 21, 18, 25, 0.99);
	

	XYIntervalSeriesCollection res = new XYIntervalSeriesCollection();
	res.addSeries( s );
	res.addSeries( s2 );
        
        return res;
    }
    
    /**
     * Creates a sample chart.
     * 
     * @param dataset  the dataset.
     * 
     * @return The chart.
     */
    private JFreeChart createChart(final XYIntervalSeriesCollection dataset) {
	NumberAxis xAxis = new NumberAxis("X");
	NumberAxis yAxis = new NumberAxis("Y");
	XYErrorRenderer rend = new BoxRenderer();
	rend.setBaseLinesVisible( false );
	rend.setBaseShapesVisible( true );

	XYPlot p = new XYPlot( dataset, xAxis, yAxis, rend);
	    
	JFreeChart chart = new JFreeChart( "pianoroll demo", p);
	
        return chart;
    }

    /**
     * Starting point for the demonstration application.
     *
     * @param args  ignored.
     */
    public static void main(final String[] args) {

        final SampleViz demo = new SampleViz("FLAZZ");
        demo.pack();
        RefineryUtilities.centerFrameOnScreen(demo);
        demo.setVisible(true);

    }

}
