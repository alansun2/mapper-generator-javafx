
package com.alan344.utils;

import org.springframework.beans.BeansException;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import java.util.Optional;

/**
 * @author AlanSun
 * @since 2021/8/23 21:11
 **/
@Component
public class SpringHelper implements ApplicationContextAware {

    private static ApplicationContext context = null;

    @Override
    public void setApplicationContext(@NonNull ApplicationContext applicationContext) throws BeansException {
        SpringHelper.context = applicationContext;
    }

    /**
     * 返回spring 上下文
     */
    public static ApplicationContext getApplicationContext() {
        return context;
    }

    /**
     * 获取对象
     */
    @NonNull
    public static <T> T getBean(String beanName, Class<T> clazz) {
        if (context == null) {
            throw new RuntimeException("未初始化完成不能调用该方法");
        }

        Object bean = context.getBean(beanName);
        return clazz.cast(bean);
    }

    /**
     * 获取对象
     */
    @NonNull
    public static <T> T getBean(Class<T> clazz) {
        if (context == null) {
            throw new RuntimeException("未初始化完成不能调用该方法");
        }

        return context.getBean(clazz);
    }


    /**
     * 获取对象
     */
    @NonNull
    public static <T> Optional<T> getBeanOpt(Class<T> clazz) {
        if (context == null) {
            return Optional.empty();
        }

        final T bean = context.getBean(clazz);
        return Optional.of(bean);
    }

}
