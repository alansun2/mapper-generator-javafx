package com.alan344.constants;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import lombok.Getter;

/**
 * @author AlanSun
 * @date 2020/9/9 15:39
 */
@Getter
public enum ConditionEnum {
    /**
     * like
     */
    like("like"),
    eq("="),
    gt(">"),
    ge(">="),
    lt("<"),
    le("<="),
    empty(""),
    ;

    private String symbol;

    ConditionEnum(String symbol) {
        this.symbol = symbol;
    }

    public static ObservableList<ConditionEnum> getAll() {
        ObservableList<ConditionEnum> observableList = FXCollections.observableArrayList();
        observableList.addAll(empty, like, eq, gt, ge, lt, le);
        return observableList;
    }
}
