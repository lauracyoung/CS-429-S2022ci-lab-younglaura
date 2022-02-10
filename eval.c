/**************************************************************************
 * C S 429 EEL interpreter
 * 
 * eval.c - This file contains the skeleton of functions to be implemented by
 * you. When completed, it will contain the code used to evaluate an expression
 * based on its AST.
 * 
 * Copyright (c) 2021. S. Chatterjee, X. Shen, T. Byrd. All rights reserved.
 * May not be used, modified, or copied without permission.
 **************************************************************************/ 

#include "ci.h"

/* Function interfaces */

/* Returns true if the given token is a binary operator and false otherwise */
extern bool is_binop(token_t);
/* Returns true if the given token is a unary operator and false otherwise */
extern bool is_unop(token_t);
/* It might be helpful to note that TOK_QUESTION is the only ternary operator. */

char *strrev(char *str);


/* infer_type() - set the type of a non-root node based on the types of children
 * Parameter: A node pointer, possibly NULL.
 * Return value: None.
 * Side effect: The type field of the node is updated.
 * (STUDENT TODO)
 */

static void infer_type(node_t *nptr) {
    // check running status - you can ignore this
    if (terminate || ignore_input) return;

    if (nptr) {
        for (int i = 0; i < 3; i++) {
            if (nptr->children[i])
                infer_type(nptr->children[i]);
        }
        switch (nptr->node_type) {
            // For each week, you will also need to include error checking for each type.
            // Week 1 TODO: Implement type inference for all operators on int and bool types.
            // Week 2 TODO: Extend type inference to handle operators on string types.
            // Week 3 TODO: Implement tpye evaluation for variables.
        case NT_INTERNAL:
            switch (nptr->tok) {
            // For reference, the identity (do nothing) operator is implemented for you.
            case TOK_IDENTITY:
                nptr->type = nptr->children[0]->type; 
                break;
            case TOK_ID:
                nptr->type = ID_TYPE;
                break;
            case TOK_NUM:
                nptr->type = INT_TYPE;
                break;
            case TOK_STR:
                nptr->type = STRING_TYPE;
                break;
            case TOK_PLUS:
                if (nptr->children[0] && nptr->children[1] 
                && nptr->children[0]->type == STRING_TYPE 
                && nptr->children[1]->type == STRING_TYPE) {
                    nptr->type = STRING_TYPE;
                } else if (nptr->children[0] && nptr->children[1] 
                && nptr->children[0]->type == INT_TYPE 
                && nptr->children[1]->type == INT_TYPE) {
                    nptr->type = INT_TYPE;
                } else {
                    handle_error(ERR_TYPE);
                }
                break;
            case TOK_TIMES:
                if (nptr->children[0] && nptr->children[1] 
                && nptr->children[0]->type == STRING_TYPE 
                && nptr->children[1]->type == INT_TYPE) {
                    nptr->type = STRING_TYPE;
                    break;
                } else if (nptr->children[0] && nptr->children[1] 
                && nptr->children[0]->type == INT_TYPE 
                && nptr->children[1]->type == INT_TYPE) {
                    nptr->type = INT_TYPE;
                } else {
                    handle_error(ERR_TYPE);
                }
                break;
            case TOK_BMINUS:
            case TOK_MOD:
            case TOK_DIV:
                if (nptr->children[0] && nptr->children[1] &&
                nptr->children[0]->type == INT_TYPE && 
                nptr->children[1]->type == INT_TYPE) {
                    nptr->type = INT_TYPE;
                } else {
                    handle_error(ERR_TYPE);
                }
                break;
            case TOK_LT:
            case TOK_GT:
            case TOK_EQ:
                if (nptr->children[0] && nptr->children[1] &&
                nptr->children[0]->type == INT_TYPE && nptr->children[1]->type == INT_TYPE) 
                    nptr->type = BOOL_TYPE;
                else if (nptr->children[0] && nptr->children[1] &&
                nptr->children[0]->type == STRING_TYPE && nptr->children[1]->type == STRING_TYPE) 
                    nptr->type = BOOL_TYPE;
                else 
                    handle_error(ERR_TYPE);
                break;
            case TOK_AND:
            case TOK_OR:
                if (nptr->children[0] && nptr->children[1] && nptr->children[0]->type == BOOL_TYPE 
                 && nptr->children[1]->type == BOOL_TYPE )
                        nptr->type = BOOL_TYPE;
                else 
                    handle_error(ERR_TYPE);
                break;
            case TOK_UMINUS:
                if (nptr->children[0] && nptr->children[0]->type == INT_TYPE) {
                    nptr->type = INT_TYPE;
                } else if (nptr->children[0] && nptr->children[0]->type == STRING_TYPE) {
                    nptr->type = STRING_TYPE;
                }
                else
                    handle_error(ERR_TYPE);
                break;
            case TOK_QUESTION:
            case TOK_COLON:\
                if (nptr->children[0] && nptr->children[1] && nptr->children[2]
                && nptr->children[0]->type == BOOL_TYPE)
                  if(nptr->children[1]->type == INT_TYPE && nptr->children[2]->type == INT_TYPE)
                    nptr->type = INT_TYPE;
                  else if (nptr->children[1]->type == BOOL_TYPE && nptr->children[2]->type == BOOL_TYPE)
                    nptr->type = BOOL_TYPE;
                  else if (nptr->children[1]->type == STRING_TYPE && nptr->children[2]->type == STRING_TYPE)
                    nptr->type = STRING_TYPE;
                  else
                    handle_error(ERR_TYPE);
                else   
                    handle_error(ERR_TYPE);
                break;
            case TOK_TRUE:
            case TOK_FALSE:
            case TOK_NOT:
                if (nptr->children[0] && nptr->children[0]->type == BOOL_TYPE)
                    nptr->type = BOOL_TYPE;
                else    
                    handle_error(ERR_TYPE);
                break;
            default:
                break;
            }
        case NT_LEAF:
            if (nptr->tok == TOK_NUM)
                nptr->type = INT_TYPE;
            else if (nptr->tok == TOK_TRUE || nptr->tok == TOK_FALSE)
                nptr->type = BOOL_TYPE;
            // else 
            //     handle_error(ERR_TYPE);
            break;
        default:
            break;
        }
    }
    return;
    
}

/* infer_root() - set the type of the root node based on the types of children
 * Parameter: A pointer to a root node, possibly NULL.
 * Return value: None.
 * Side effect: The type field of the node is updated. 
 */

static void infer_root(node_t *nptr) {
    if (nptr == NULL) return;
    // check running status
    if (terminate || ignore_input) return;

    // check for assignment
    if (nptr->type == ID_TYPE) {
        infer_type(nptr->children[1]);
    } else {
        for (int i = 0; i < 3; ++i) {
            infer_type(nptr->children[i]);
        }
        if (nptr->children[0] == NULL) {
            logging(LOG_ERROR, "failed to find child node");
            return;
        }
        nptr->type = nptr->children[0]->type;
    }
    return;
}

/* eval_node() - set the value of a non-root node based on the values of children
 * Parameter: A node pointer, possibly NULL.
 * Return value: None.
 * Side effect: The val field of the node is updated.
 * (STUDENT TODO) 
 */

static void eval_node(node_t *nptr) {
    // check running status - you can ignore this.
    if (terminate || ignore_input) return;
    if (nptr) {
        if (nptr->tok == TOK_QUESTION) {
            if (nptr->children[0]->tok == TOK_TRUE) {
                eval_node(nptr->children[1]);
            }
            else {
                eval_node(nptr->children[2]);
            }
        }else {
            for (int i = 0; i < 3; i++) {
            if (nptr->children[i])
                eval_node(nptr->children[i]);
            }
         }
        switch (nptr->node_type) {
            case NT_INTERNAL:
                // Week 1 TODO: Implement evaluation for all operators on int and bool types.
                // Week 2 TODO: Extend evaluation to handle operators on string types.
                if (is_unop(nptr->tok)) {
                    switch (nptr->tok) {
                        case TOK_UMINUS:
                            if (nptr->type == INT_TYPE) {
                                nptr->val.ival = -1 * nptr->children[0]->val.ival;
                            } else if (nptr->type == STRING_TYPE) {
                                nptr->val.sval = malloc(strlen(nptr->children[0]->val.sval) + 1);
                                strrev(nptr->children[0]->val.sval);
                                strcpy(nptr->val.sval, nptr->children[0]->val.sval);
                            }
                            break;
                        case TOK_NOT:
                            if (nptr->children[0]->tok == TOK_FALSE) {
                                nptr->children[0]->tok = TOK_TRUE;
                                nptr->children[0]->val.bval = true;
                            } else {
                                nptr->children[0]->tok = TOK_FALSE;
                                nptr->children[0]->val.bval = false;
                            }
                            break;
                        default:
                            break;
                    }
                }
                if (is_binop(nptr->tok)) {
                    switch (nptr->tok) {
                        case TOK_PLUS:
                            if (nptr->type == INT_TYPE) {
                                nptr->val.ival = nptr->children[0]->val.ival + nptr->children[1]->val.ival;
                            } else if (nptr->type == STRING_TYPE) {
                                nptr->val.sval = malloc(strlen(nptr->children[0]->val.sval) + sizeof(nptr->children[1]->val.sval));
                                nptr->val.sval = strcpy(nptr->val.sval, nptr->children[0]->val.sval);
                                nptr->val.sval = strcat(nptr->val.sval, nptr->children[1]->val.sval);
                            }
                            break;
                        case TOK_BMINUS:
                            nptr->val.ival = nptr->children[0]->val.ival - nptr->children[1]->val.ival;
                            break;
                        case TOK_TIMES:
                            if (nptr->type == INT_TYPE) {
                                nptr->val.ival = nptr->children[0]->val.ival * nptr->children[1]->val.ival;
                            } else if (nptr->type == STRING_TYPE) {
                                nptr->val.sval = malloc((strlen(nptr->children[0]->val.sval) * nptr->children[1]->val.ival) + 1);
                                nptr->val.sval[0] = '\0';
                                for (int i = 0; i < nptr->children[1]->val.ival; i++) {
                                    strcat(nptr->val.sval, nptr->children[0]->val.sval);
                                }
                            }
                            break;
                        case TOK_DIV:
                            if (nptr->children[1]->val.ival == 0)
                                handle_error(ERR_EVAL);
                            else
                                nptr->val.ival = nptr->children[0]->val.ival / nptr->children[1]->val.ival;
                            break;
                        case TOK_MOD:
                            if (nptr->children[1]->val.ival == 0)
                                handle_error(ERR_EVAL);
                            else
                                nptr->val.ival = nptr->children[0]->val.ival % nptr->children[1]->val.ival;
                            break;
                        case TOK_AND:
                            nptr->val.bval = nptr->children[0]->val.bval && nptr->children[1]->val.bval;
                            break;
                        case TOK_OR:
                            nptr->val.bval = nptr->children[0]->val.bval || nptr->children[1]->val.bval;
                            break;
                        case TOK_LT:
                            if (nptr->children[0]->type == INT_TYPE) {
                                nptr->val.bval = nptr->children[0]->val.ival < nptr->children[1]->val.ival;
                            } else if (nptr->children[0]->type == STRING_TYPE) {
                                nptr->val.bval = strcmp(nptr->children[0]->val.sval, nptr->children[1]->val.sval) < 0;
                            }
                            break;
                        case TOK_GT:
                            if (nptr->children[0]->type == INT_TYPE) {
                                nptr->val.bval = nptr->children[0]->val.ival > nptr->children[1]->val.ival;
                            } else if (nptr->children[0]->type == STRING_TYPE) {
                                nptr->val.bval = strcmp(nptr->children[0]->val.sval, nptr->children[1]->val.sval) > 0;
                            }
                            break;
                        case TOK_EQ:
                            if (nptr->children[0]->type == INT_TYPE) {
                                nptr->val.bval = (nptr->children[0]->val.ival == nptr->children[1]->val.ival);
                            } else if (nptr->children[0]->type == STRING_TYPE) {
                                nptr->val.bval = (strcmp(nptr->children[0]->val.sval, nptr->children[1]->val.sval) == 0);
                            }
                            break;
                        default:
                            break;
                    }
                }
                if (nptr->tok == TOK_QUESTION) {
                    if (nptr->children[0]->tok == TOK_TRUE) {
                        nptr->val = nptr->children[1]->val;
                    }
                    else {
                        nptr->val = nptr->children[2]->val;
                    }
                }
                // For reference, the identity (do-nothing) operator has been implemented for you.
                if (nptr->tok == TOK_IDENTITY) {
                    if (nptr->type == STRING_TYPE) {
                        // Week 2 TODO: You'll need to make a copy of the string.
                    } else {
                        nptr->val.ival = nptr->children[0]->val.ival;
                    }
                }
                break;
            case NT_LEAF:
                break;
            default:
                break;
        }
    }   
    return;
}

/* eval_root() - set the value of the root node based on the values of children 
 * Parameter: A pointer to a root node, possibly NULL.
 * Return value: None.
 * Side effect: The val dield of the node is updated. 
 */

void eval_root(node_t *nptr) {
    if (nptr == NULL) return;
    // check running status
    if (terminate || ignore_input) return;

    // check for assignment
    if (nptr->type == ID_TYPE) {
        eval_node(nptr->children[1]);
        if (terminate || ignore_input) return;
        
        if (nptr->children[0] == NULL) {
            logging(LOG_ERROR, "failed to find child node");
            return;
        }
        put(nptr->children[0]->val.sval, nptr->children[1]);
        return;
    }

    for (int i = 0; i < 2; ++i) {
        eval_node(nptr->children[i]);
    }
    if (terminate || ignore_input) return;
    
    if (nptr->type == STRING_TYPE) {
        (nptr->val).sval = (char *) malloc(strlen(nptr->children[0]->val.sval) + 1);
        if (! nptr->val.sval) {
            logging(LOG_FATAL, "failed to allocate string");
            return;
        }
        strcpy(nptr->val.sval, nptr->children[0]->val.sval);
    } else {
        nptr->val.ival = nptr->children[0]->val.ival;
    }
    return;
}

/* infer_and_eval() - wrapper for calling infer() and eval() 
 * Parameter: A pointer to a root node.
 * Return value: none.
 * Side effect: The type and val fields of the node are updated. 
 */

void infer_and_eval(node_t *nptr) {
    infer_root(nptr);
    eval_root(nptr);
    return;
}

/* strrev() - helper function to reverse a given string 
 * Parameter: The string to reverse.
 * Return value: The reversed string. The input string is not modified.
 * (STUDENT TODO)
 */

char *strrev(char *str) {
   for (int i = 0; i < strlen(str)/2; i++) {
       int front = i;
       int back = strlen(str) - i - 1;
       char temp = str[front];
       str[front] = str[back];
       str[back] = temp;
   }
   return str;
}

/* cleanup() - frees the space allocated to the AST
 * Parameter: The node to free.
 */
void cleanup(node_t *nptr) {
    // Week 2 TODO: Recursively free each node in the AST
    if (!nptr) {
        return;
    }
    if(nptr->children[0]) {
        cleanup(nptr->children[0]);
    }
    if(nptr->children[1]) {
        cleanup(nptr->children[1]);
    }
    if(nptr->children[2]) {
        cleanup(nptr->children[2]);
    }
    if(nptr->type == STRING_TYPE) {
        free(nptr->val.sval);
    } 
    if (nptr) {
        free(nptr);
    } 
    return;
}
