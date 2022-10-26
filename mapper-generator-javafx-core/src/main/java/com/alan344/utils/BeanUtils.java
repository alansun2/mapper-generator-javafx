package com.alan344.utils;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;

/**
 * bean工具类.
 *
 * @author demon
 * @date 2016/5/11 9:40
 */
public class BeanUtils {

    /**
     * 判断两个相同类型的对象的值是否不存在不同的值
     *
     * @param older 类1
     * @param newer 类2
     * @param <T>   类型
     * @return true ： 两个对象完全相同， false : 存在不同的值
     */
    public static <T> boolean checkPropertyOfBean(T older, T newer) {
        Class<?> clazz = older.getClass();
        Field[] fields = clazz.getDeclaredFields();
        try {
            for (Field field : fields) {
                PropertyDescriptor pd = new PropertyDescriptor(field.getName(), clazz);
                Method getMethod = pd.getReadMethod();
                Object o1 = getMethod.invoke(older);
                Object o2 = getMethod.invoke(newer);
                //避免空指针异常
                Object s1 = o1 == null ? "" : o1;
                //避免空指针异常
                Object s2 = o2 == null ? "" : o2;
                if (!s1.equals(s2)) {
                    return false;
                }
            }
        } catch (Exception e) {
            return false;
        }

        return true;
    }
}
