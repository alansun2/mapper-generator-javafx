package com.alan344;

import com.alan344.exception.BizException;
import com.alan344.factory.DialogFactory;
import com.alan344.view.MainView;
import javafx.application.Application;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.InvocationTargetException;

/**
 * @author AlanSun
 * @date 2019/8/7 17:07
 */
@Slf4j
public class MapperGenApplication {

    public static void main(String[] args) {
        // 捕捉未处理的异常
        Thread.setDefaultUncaughtExceptionHandler((t, e) -> {
            if (e instanceof InvocationTargetException) {
                final Throwable cause = e.getCause();
                if (!(cause instanceof BizException)) {
                    DialogFactory.exceptionDialog(cause);
                    log.error("捕捉到未处理的异常", cause);
                }
            } else if (!(e instanceof BizException)) {
                DialogFactory.exceptionDialog(e);
                log.error("捕捉到未处理的异常", e);
            }
        });
        Application.launch(MainView.class, args);
    }
}
