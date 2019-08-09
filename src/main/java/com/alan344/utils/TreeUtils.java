package com.alan344.utils;

import javafx.scene.control.TreeItem;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 22:27
 */
public class TreeUtils {

    public static <T> TreeItem<T> add2Tree(T content, TreeItem<T> parent) {
        TreeItem<T> item = new TreeItem<>(content);
        parent.getChildren().add(item);
        return item;
    }
}
