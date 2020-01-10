package com.alan344.utils;

import javafx.scene.control.TreeItem;

/**
 * @author ：AlanSun
 * @date ：2019/8/8 22:27
 */
public class TreeUtils {

    /**
     * 把 content 包装成 TreeItem，然后放入 parent
     *
     * @param content 需要封装的内容
     * @param parent  外层
     * @return content 被封装后 TreeItem
     */
    public static <T> TreeItem<T> add2Tree(T content, TreeItem<T> parent) {
        TreeItem<T> item = new TreeItem<>(content);
        parent.getChildren().add(item);
        return item;
    }
}
