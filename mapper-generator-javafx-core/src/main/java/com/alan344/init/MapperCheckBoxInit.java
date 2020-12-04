package com.alan344.init;

import com.alan344.constants.BaseConstants;
import javafx.collections.ObservableList;
import javafx.scene.Node;
import javafx.scene.control.CheckBox;
import javafx.scene.layout.HBox;
import org.springframework.stereotype.Service;

/**
 * @author AlanSun
 * @date 2020/4/2 18:15
 */
@Service
public class MapperCheckBoxInit {

    /**
     * 初始化头的 hbox，设置监听
     *
     * @param mapperCheckBoxHBox1 第一行的 hbox
     * @param mapperCheckBoxHBox2 第二行的 hbox
     */
    public void checkBoxInit(HBox mapperCheckBoxHBox1, HBox mapperCheckBoxHBox2) {
        final ObservableList<Node> checkBoxChildren1 = mapperCheckBoxHBox1.getChildren();
        for (int i = 0; i < checkBoxChildren1.size(); i++) {
            Node checkBoxChild = checkBoxChildren1.get(i);
            final CheckBox checkBoxChild1 = (CheckBox) checkBoxChild;
            int finalI = i;
            checkBoxChild1.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(0, finalI, newValue));
        }

        final ObservableList<Node> checkBoxChildren2 = mapperCheckBoxHBox2.getChildren();
        for (int i = 0; i < checkBoxChildren2.size(); i++) {
            Node checkBoxChild = checkBoxChildren2.get(i);
            final CheckBox checkBoxChild1 = (CheckBox) checkBoxChild;
            int finalI = i;
            checkBoxChild1.selectedProperty().addListener((observable, oldValue, newValue) -> checkBoxAction(1, finalI, newValue));
        }
    }

    private void checkBoxAction(int columnIndex, int rowIndex, boolean selected) {
        if (!BaseConstants.selectedCheckBoxVBox.isEmpty()) {
            BaseConstants.selectedCheckBoxVBox.forEach(vBox -> ((CheckBox) ((HBox) vBox.getChildren().get(columnIndex)).getChildren().get(rowIndex)).setSelected(selected));
        }
    }
}
