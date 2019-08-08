package com.alan344.utils;

import javafx.scene.control.TreeItem;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 22:27
 */
public class TreeUtils {

    public static void add2Tree(String treeName, TreeItem<String> node) {
        TreeItem<String> item = new TreeItem<>(treeName);
        node.getChildren().add(item);
    }
}
