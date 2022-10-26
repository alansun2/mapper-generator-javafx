package com.alan344.factory;

import javafx.fxml.FXMLLoader;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.BeanFactory;

import java.io.IOException;

/**
 * @author AlanSun
 * @date 2020/9/9 9:30
 */
@Slf4j
public class FxmlLoadFactory {

    public static <T> T create(String path, BeanFactory beanFactory) {
        FXMLLoader fxmlLoader = new FXMLLoader();

        fxmlLoader.setLocation(FxmlLoadFactory.class.getResource(path));
        fxmlLoader.setControllerFactory(beanFactory::getBean);
        T node;
        try {
            node = fxmlLoader.load();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
        return node;
    }
}
