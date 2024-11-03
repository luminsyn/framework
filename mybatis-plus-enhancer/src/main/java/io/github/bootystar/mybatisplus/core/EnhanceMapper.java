package io.github.bootystar.mybatisplus.core;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.baomidou.mybatisplus.core.metadata.IPage;
import io.github.bootystar.mybatisplus.injection.Injector;
import org.apache.ibatis.annotations.Param;

import java.util.List;
import java.util.Map;


/**
 * mapper
 * @author bootystar
 */
public interface EnhanceMapper<T,V>  extends BaseMapper<T> {

    List<V> listByMap(@Param("map") Map<String,Object> map, IPage<V> page);

    List<V> listByCondition(@Param("injector") Injector injector, @Param("page") IPage<V> page);

    /*
    select *
        from `user` a
        left join user11 b on a.`id` = b.`id`
        <where>
            and a.`deleted` = 0
            <if test="injector!=null and injector.conditions!=null and injector.conditions.size()>0">
            and (
            <trim suffixOverrides="AND|OR">
                    <foreach collection="injector.conditions"  item="item" index="index">
                        <choose>
                            <when test="item.symbol.contains('in'.toString())">
                                ${item.field} ${item.symbol}
                                <foreach collection="item.value" item="val" separator="," open="(" close=")">
                                    #{val}
                                </foreach>
                            </when>
                            <when test="item.symbol.contains('is'.toString() )">
                                ${item.field} ${item.symbol}
                            </when>
                            <otherwise>
                                ${item.field} ${item.symbol} #{item.value}
                            </otherwise>
                        </choose>
                        ${injector.connector}
                    </foreach>
            </trim>
            )
            </if>
        </where>
     */

    /*
    <sql id="childFragment">
      <foreach collection="children" item="child" open="(" close=")" separator=" OR ">
        <if test="child.condition != null">
          AND child.condition = #{child.condition}
        </if>
        <include refid="childFragment" />
      </foreach>
    </sql>

    <select id="selectByExample" parameterType="Example" resultMap="BaseResultMap">
      SELECT * FROM table
      <where>
        <if test="example.condition != null">
          AND example.condition = #{example.condition}
        </if>
        <include refid="childFragment" />
      </where>
    </select>
     */

}
