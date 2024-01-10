import io.github.bootystar.tool.MapUtil;
import org.junit.jupiter.api.Test;

import java.util.LinkedList;
import java.util.List;

/**
 * @author booty
 *
 */
public class MapUtilTest {



    @Test
    void t1 (){
//        MapUtil.MapPoint point2 =
//        MapUtil.MapPoint point3 = new MapUtil.MapPoint(10,1);
//        MapUtil.MapPoint point4 =
//        MapUtil.MapPoint point4 = new MapUtil.MapPoint(10,10);
        MapUtil.MapPoint[] points ={
                new MapUtil.MapPoint(1,1),
                new MapUtil.MapPoint(1,2),
                new MapUtil.MapPoint(2,2),
                new MapUtil.MapPoint(2,3),
                new MapUtil.MapPoint(1,3),
                new MapUtil.MapPoint(1,4),
                new MapUtil.MapPoint(2,4),
                new MapUtil.MapPoint(4,2),
                new MapUtil.MapPoint(3,2),
                new MapUtil.MapPoint(3,1)
        };

        MapUtil.MapPoint point = new MapUtil.MapPoint(3.5,2.1);

        boolean in = MapUtil.isPointInPolygon(point, points);
        System.out.println(in);
    }





}
