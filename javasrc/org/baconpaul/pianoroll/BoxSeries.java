package org.baconpaul.pianoroll;

import org.jfree.data.ComparableObjectItem;
import org.jfree.data.ComparableObjectSeries;
import org.jfree.data.general.SeriesChangeEvent;

public class BoxSeries extends org.jfree.data.xy.XYIntervalSeries
{
    public BoxSeries( String t )
    {
	super( t );
    }
    
    public static class BoxDataItem extends org.jfree.data.xy.XYIntervalDataItem 
    {
	private double pct;
	public BoxDataItem( double x, double xL, double xH,
			    double y, double yL, double yH,
			    double pct )
	{
	    super( x, xL, xH, y, yL, yH );
	    this.pct = pct;
	}
	public double getPct() { return this.pct; }
	public int compareTo( Object o1 )
	{
	    int sct = super.compareTo( o1 );
	    if( sct == 0 && o1 instanceof BoxDataItem )
	    {
		double pp = ((BoxDataItem)o1).getPct();
		if( pp < this.pct ) return -1;
		if( pp > this.pct ) return 1;
		return 0;
	    }
	    else
	    {
		return sct;
	    }
	}
    }

    public void add( double x, double xL, double xH,
		     double y, double yL, double yH,
		     double pct ) // a number between 0 and 1 to indicate "intensity"
    {
	add( new BoxDataItem( x, xL, xH, y, yL, yH, pct ), true );
    }

    public double getPct( int index )
    {
	BoxDataItem item = (BoxDataItem)getDataItem(index);
	return item.getPct();
    }
}
