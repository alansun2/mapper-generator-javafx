package com.alan344.utils;

import java.beans.PropertyDescriptor;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.List;

/**
 * bean工具类.
 *
 * @author demon
 * @since 2016/5/11 9:40
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
                if (List.class.isAssignableFrom(field.getType())) {
                    PropertyDescriptor pd = new PropertyDescriptor(field.getName(), clazz);
                    Method getMethod = pd.getReadMethod();
                    List c1 = (List) getMethod.invoke(older);
                    List c2 = (List) getMethod.invoke(newer);
                    if (c1.size() != c2.size()) {
                        return false;
                    }
                    for (int i = 0; i < c1.size(); i++) {
                        final boolean b = checkPropertyOfBean(c1.get(i), c2.get(i));
                        if (!b) {
                            return false;
                        }
                    }
                } else if (field.getType().isPrimitive() || CharSequence.class.isAssignableFrom(field.getType()) || Boolean.class.isAssignableFrom(field.getType()) || Number.class.isAssignableFrom(field.getType())) {
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
                } else if (field.getType().isEnum()) {
                    PropertyDescriptor pd = new PropertyDescriptor(field.getName(), clazz);
                    Method getMethod = pd.getReadMethod();
                    Object o1 = getMethod.invoke(older);
                    Object o2 = getMethod.invoke(newer);
                    //避免空指针异常
                    Object s1 = o1 == null ? "" : o1;
                    //避免空指针异常
                    Object s2 = o2 == null ? "" : o2;
                    if (s1 != s2) {
                        return false;
                    }
                } else {
                    PropertyDescriptor pd = new PropertyDescriptor(field.getName(), clazz);
                    Method getMethod = pd.getReadMethod();
                    Object o1 = getMethod.invoke(older);
                    Object o2 = getMethod.invoke(newer);
                    //避免空指针异常
                    Object s1 = o1 == null ? "" : o1;
                    //避免空指针异常
                    Object s2 = o2 == null ? "" : o2;
                    final boolean b = checkPropertyOfBean(s1, s2);
                    if (!b) {
                        return false;
                    }
                }
            }
        } catch (Exception e) {
            return false;
        }

        return true;
    }
}
