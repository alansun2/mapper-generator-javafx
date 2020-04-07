package com.alan344.init;

import com.alan344.controller.MainController;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyEvent;
import org.springframework.stereotype.Service;

import javax.annotation.Resource;

/**
 * @author AlanSun
 * @date 2020/4/7 14:45
 */
@Service
public class TableTextFieldListener {
    @Resource
    private MainController mainController;

    public void escListener(KeyEvent event) {
        KeyCode code = event.getCode();
        if (KeyCode.ESCAPE.equals(code)) {
            mainController.getTableFindTextField().setText("");
            mainController.getTreeViewDataSource().requestFocus();
        }
    }

    public void ctrlFListener(KeyEvent event) {
        KeyCode code = event.getCode();
        // ctrl + F
        if (KeyCode.F.equals(code)) {
            if (event.isControlDown()) {
                mainController.getTableFindTextField().requestFocus();
            }
        } else if (KeyCode.ESCAPE.equals(code)) {
            mainController.getTableFindTextField().setText("");
        }
    }
}
