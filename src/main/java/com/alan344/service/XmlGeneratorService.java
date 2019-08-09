package com.alan344.service;

import com.alan344.bean.DataItem;
import javafx.collections.ObservableList;
import javafx.scene.control.TreeItem;
import org.springframework.stereotype.Service;

/**
 * @author AlanSun
 * @date 2019/8/9 16:19
 */
@Service
public class XmlGeneratorService {

    public void generatorXml(ObservableList<TreeItem<DataItem>> tableTreeItems) {
        for (TreeItem<DataItem> tableTreeItem : tableTreeItems) {
        }
    }
}