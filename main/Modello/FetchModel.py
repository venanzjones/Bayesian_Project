import os
from cmdstanpy import CmdStanModel

class FetchModel:
    """
    This class takes as input the name of the model and fetches it from the ./stan folder.
    The list of available models is stored in the model_list attribute. If the requested model is not available, the class raises an error.
    The method compile() returns the CmdStanModel compiled model.
    To add new models, preferibly add the stan file directly in the folder ./stan.
    Alternatively it can be done by adding the add_model = True argument when initializing and providing the model_code string.
    To remove a model, it is enough to delete the corresponding file in the folder.
    It is possible to retrieve the code of the model by calling the get_code() method.
    It is also possible to update the model by calling the update_model() method and providing the new model_code string.
    """
    def __init__(self, model_name = None, add_model = False, model_code = None, reset = False):
        if not os.path.exists('./stan'):
            os.makedirs('./stan')
        if not os.path.exists('./stan/compiled'):
            os.makedirs('./stan/compiled')
        if reset:
            self.reset_compiled()
            return None
        self.model_name = model_name
        self.model_list = []
        self.update_list()
        if add_model:
            if self.model_name in self.model_list:
                raise ValueError('\nThe model you want to add is {}, which is already available.\nPlease choose another name.\nThe list of names already in use is:\n{}'.format(self.model_name, self.model_list))
            self.stan_file = "./stan/{}.stan".format(self.model_name)
            with open(self.stan_file, 'w') as f:
                print(model_code, file=f)
            self.update_list()
            print('The model {} has been added.\nThe updated model list is the following:\n{}'.format(self.model_name, self.model_list))
        else:
            if self.model_name in self.model_list:
                self.stan_file = "./stan/{}.stan".format(self.model_name)
            else:
                raise ValueError('\nThe requested model is {}, which is not available.\nPlease choose one among the following:\n{}'.format(self.model_name, self.model_list))
            return None
    
    def update_list(self):
        for file in os.listdir('./stan'):
            if file.endswith('.stan'):
                self.model_list.append(file.split('.')[0])
        for file in os.listdir('./stan/compiled'):
            if file.endswith('.exe') or file.endswith('.hpp'):
                if not os.path.exists('./stan/{}'.format(file.split('.')[0]+'.stan')):
                    os.remove('./stan/compiled/{}'.format(file))
        return self

    def compile(self):
        exe_file = './stan/compiled/{}.exe'.format(self.model_name)
        hpp_file = './stan/compiled/{}.hpp'.format(self.model_name)
        if not os.path.exists(exe_file):
            CmdStanModel(stan_file=self.stan_file)
            os.rename('./stan/{}.hpp'.format(self.model_name), hpp_file)
            os.rename('./stan/{}.exe'.format(self.model_name), exe_file)
        if os.path.getmtime(self.stan_file) > os.path.getmtime(exe_file):
            CmdStanModel(stan_file=self.stan_file)
            os.replace('./stan/{}.exe'.format(self.model_name), exe_file)
            os.replace('./stan/{}.hpp'.format(self.model_name), hpp_file)
        return CmdStanModel(stan_file=self.stan_file, exe_file=exe_file)
    
    def get_code(self):
        with open(self.stan_file, 'r') as f:
            return f.read()
        
    def update_model(self, model_code):
        with open(self.stan_file, 'w') as f:
            print(model_code, file=f)
        print('The model {} has been updated.\nThe updated model list is the following:\n{}'.format(self.model_name, self.model_list))
        return self
    
    def reset_compiled(self, all=False):
        for file in os.listdir('./stan'):
            if file.endswith('.exe') or file.endswith('.hpp'):
                os.remove('./stan/{}'.format(file))
        for file in os.listdir('./stan/compiled'):
            if file.endswith('.exe') or file.endswith('.hpp'):
                os.remove('./stan/compiled/{}'.format(file))
        return None
