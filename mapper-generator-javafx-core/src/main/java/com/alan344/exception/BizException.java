package com.alan344.exception;


import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.io.Serial;

/**
 * @author AlanSun
 * @since 2017年3月24日 下午2:32:30
 */
@Getter
@Setter
@NoArgsConstructor
public class BizException extends RuntimeException {

    @Serial
    private static final long serialVersionUID = -5328505822127772820L;

    private int errorCode;

    private String errorMsg;

    private int httpStatus;

    private Object data;

    public BizException(int errorCode, String errorMsg, int httpStatus, Object data) {
        this.errorCode = errorCode;
        this.errorMsg = errorMsg;
        this.httpStatus = httpStatus;
        this.data = data;
    }


    public BizException(Exception e) {
        super(e);
        this.errorCode = 1;
    }

    public BizException(int errorCode, String msg) {
        super(msg);
        this.errorCode = errorCode;
        this.errorMsg = msg;
    }

    public BizException(int errorCode, String msg, int httpStatus) {
        super(msg);
        this.errorCode = errorCode;
        this.errorMsg = msg;
        this.httpStatus = httpStatus;
    }

    public BizException(int errorCode, String msg, Object data) {
        super(msg);
        this.errorCode = errorCode;
        this.errorMsg = msg;
        this.data = data;
    }

    public BizException(int errorCode, String msg, Throwable cause) {
        super(msg, cause);
        this.errorCode = errorCode;
        this.errorMsg = msg;
    }

    public BizException(String msg) {
        super(msg);
        this.errorCode = 1;
        this.errorMsg = msg;
    }

    public BizException(String msg, int httpStatus) {
        super(msg);
        this.errorCode = 1;
        this.errorMsg = msg;
        this.httpStatus = httpStatus;
    }
}
