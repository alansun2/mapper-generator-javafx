package org.mybatis.generator.logging;

import org.mybatis.generator.logging.slf4j.Slf4jLoggingLogFactory;

import static org.mybatis.generator.internal.util.messages.Messages.getString;

/**
 * Factory for creating loggers.
 *
 * @author Jeff Butler
 */
public class LogFactory {
    private static AbstractLogFactory theFactory;
    public static final String MARKER = "MYBATIS-GENERATOR"; //$NON-NLS-1$

    static {
        tryImplementation(new Slf4jLoggingLogFactory());
    }

    public static Log getLog(Class<?> clazz) {
        try {
            return theFactory.getLog(clazz);
        } catch (Exception t) {
            throw new RuntimeException(getString("RuntimeError.21", //$NON-NLS-1$
                    clazz.getName(), t.getMessage()), t);
        }
    }

    private static void tryImplementation(AbstractLogFactory factory) {
        if (theFactory == null) {
            try {
                setImplementation(factory);
            } catch (LogException e) {
                // ignore
            }
        }
    }

    private static void setImplementation(AbstractLogFactory factory) {
        try {
            Log log = factory.getLog(LogFactory.class);
            if (log.isDebugEnabled()) {
                log.debug("Logging initialized using '" + factory + "' adapter."); //$NON-NLS-1$ //$NON-NLS-2$
            }
            theFactory = factory;
        } catch (Exception t) {
            throw new LogException("Error setting Log implementation.  Cause: " + t.getMessage(), t); //$NON-NLS-1$
        }
    }
}
